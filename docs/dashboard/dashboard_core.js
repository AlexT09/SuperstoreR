/*
 * dashboard_core.js — Lógica compartida del dashboard de ventas (Sample Superstore).
 *
 * La usan la vista de gráficos del dashboard 3D ("Dashboard Superstore.dc.html") y el HTML
 * autocontenido assets/dashboard.html (dashboard/build_dashboard.py la incrusta allí).
 * Las figuras llegan de Python como esqueletos de Plotly (tipo de traza, ejes, formato);
 * aquí se llenan con los datos filtrados y se escriben los KPIs y el insight de cada gráfico.
 *
 * Insights: sin filtros se muestra el texto del EDA (D.eda, tomado de tabs/eda.py); con filtros,
 * una lectura equivalente calculada con los datos filtrados.
 *
 *   const dash = SuperstoreDashboard.create(FIGS, D, { pal, config, doc: document });
 *   dash.update({ cat: -1, reg: -1, seg: -1, year: 'all' });
 *
 * Elementos que busca en `doc`: #kpis, #calidad, cada figura por su id (fig-…) y su insight
 * en #ins-<id>. Los que no existan se omiten.
 */
(function (root) {
  'use strict';

  const MES = ['Ene', 'Feb', 'Mar', 'Abr', 'May', 'Jun', 'Jul', 'Ago', 'Sep', 'Oct', 'Nov', 'Dic'];

  // ---------------------------------------------------------------- formatos
  const fmtInt = v => Math.round(v).toLocaleString('en-US');
  const money = v => {
    const a = Math.abs(v), s = v < 0 ? '−' : '';
    if (a >= 1e6) return `${s}$${(a / 1e6).toFixed(2)}M`;
    if (a >= 1e3) return `${s}$${(a / 1e3).toFixed(1)}K`;
    return `${s}$${a.toFixed(a < 100 ? 1 : 0)}`;
  };
  const pct = (v, d = 1) => `${v.toFixed(d).replace('-', '−')}%`;
  const num = (v, d = 2) => v.toFixed(d).replace('-', '−');
  const sum = a => a.reduce((x, y) => x + y, 0);
  const median = arr => {
    if (!arr.length) return null;
    const a = Float64Array.from(arr).sort(), h = a.length >> 1;
    return a.length % 2 ? a[h] : (a[h - 1] + a[h]) / 2;
  };
  const pearson = (x, y) => {
    const n = x.length;
    if (n < 3) return null;
    const mx = sum(x) / n, my = sum(y) / n;
    let sxy = 0, sxx = 0, syy = 0;
    for (let i = 0; i < n; i++) { const dx = x[i] - mx, dy = y[i] - my; sxy += dx * dy; sxx += dx * dx; syy += dy * dy; }
    return sxx && syy ? sxy / Math.sqrt(sxx * syy) : null;
  };
  const lista = items => items.length > 1 ? items.slice(0, -1).join(', ') + ' y ' + items[items.length - 1] : (items[0] || '');
  const argmax = (idx, f) => idx.reduce((a, b) => (f(b) > f(a) ? b : a));
  const argmin = (idx, f) => idx.reduce((a, b) => (f(b) < f(a) ? b : a));
  const etiquetaMes = k => `${MES[+k.slice(5, 7) - 1]} ${k.slice(2, 4)}`;   // '2017-11' -> 'Nov 17'
  const escape = t => t.replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;');

  const insight = lineas => {
    const t = lineas.filter(Boolean);
    return '<span class="tag">Insight</span>' +
      (t.length ? t.map(x => `<p>${x}</p>`).join('') : '<p>Sin datos suficientes para esta combinación de filtros.</p>');
  };

  function create(FIGS, D, opt) {
    const N = D.Sales.length;
    const YEAR = D.months.map(m => m.slice(0, 4));
    const YEARS = [...new Set(YEAR)].sort();
    const pal = opt.pal, doc = opt.doc || document;
    const draw = id => { const el = doc.getElementById(id); if (el) Plotly.react(el, FIGS[id].data, FIGS[id].layout, opt.config); };
    const setHTML = (id, html) => { const el = doc.getElementById(id); if (el) el.innerHTML = html; };

    function rowsFor(f, year) {
      const out = [];
      for (let i = 0; i < N; i++) {
        if (f.cat >= 0 && D.c[i] !== f.cat) continue;
        if (f.reg >= 0 && D.r[i] !== f.reg) continue;
        if (f.seg >= 0 && D.s[i] !== f.seg) continue;
        if (year !== 'all' && YEAR[D.m[i]] !== year) continue;
        out.push(i);
      }
      return out;
    }

    function agg(rows) {
      const v = rows.map(i => D.Sales[i]);
      return { n: v.length, s: sum(v), mean: v.length ? sum(v) / v.length : 0, med: median(v) || 0, max: v.length ? Math.max(...v) : 0 };
    }

    // ---------------------------------------------------------------- KPIs
    function kpis(f, rows) {
      const A = agg(rows);
      // comparación: año elegido vs. el anterior; con "Todos", último año vs. penúltimo
      let cur = null, prev = null, lbl = '';
      const yi = YEARS.indexOf(f.year);
      if (f.year === 'all' && YEARS.length > 1) {
        const a = YEARS[YEARS.length - 1], b = YEARS[YEARS.length - 2];
        cur = agg(rowsFor(f, a)); prev = agg(rowsFor(f, b)); lbl = `${a} vs ${b}`;
      } else if (yi > 0) {
        cur = A; prev = agg(rowsFor(f, YEARS[yi - 1])); lbl = `vs ${YEARS[yi - 1]}`;
      }
      if (prev && !prev.n) prev = null;
      const delta = k => {
        if (!prev || !prev[k]) return '<div class="d"></div>';
        const d = (cur[k] - prev[k]) / Math.abs(prev[k]) * 100;
        return `<div class="d ${d >= 0 ? 'pos' : 'neg'}">${d >= 0 ? '▲' : '▼'} ${Math.abs(d).toFixed(1)}% ${lbl}</div>`;
      };
      const k = (l, val, key) => `<div class="kpi"><div class="l">${l}</div><div class="v">${A.n ? val : '–'}</div>${delta(key)}</div>`;
      return k('Ventas', money(A.s), 's') + k('Pedidos', fmtInt(A.n), 'n') + k('Media de Sales', money(A.mean), 'mean')
        + k('Mediana de Sales', money(A.med), 'med') + k('Máximo de Sales', money(A.max), 'max');
    }

    // ---------------------------------------------------------------- gráficos
    function hist(rows) {
      const v = rows.map(i => D.Sales[i]), fig = FIGS['fig-hist'];
      fig.data[0].x = v.filter(x => x <= D.histMax);
      fig.data[0].marker = { color: pal.main, line: { color: pal.bg, width: 0.5 } };
      const A = agg(rows);
      const linea = (x, txt, color, lado) => ({
        shape: { type: 'line', xref: 'x', yref: 'paper', x0: x, x1: x, y0: 0, y1: 1, line: { color, width: 1.5, dash: 'dot' } },
        ann: { xref: 'x', yref: 'paper', x, y: 1, yanchor: 'bottom', xanchor: lado, showarrow: false, text: txt, font: { size: 11, color } },
      });
      const marcas = A.n ? [linea(A.med, `Mediana ${money(A.med)} `, pal.ink, 'right'), linea(A.mean, ` Media ${money(A.mean)}`, pal.accent, 'left')] : [];
      fig.layout.shapes = marcas.map(m => m.shape);
      fig.layout.annotations = marcas.map(m => m.ann);
      draw('fig-hist');
      if (A.n < 2) return insight([]);
      return insight([
        `La media (<b>${money(A.mean)}</b>) es ${(A.mean / A.med).toFixed(1)} veces la mediana (<b>${money(A.med)}</b>): ` +
        `la mayoría de los pedidos son de bajo valor y unos pocos pedidos muy grandes (hasta ${money(A.max)}) jalan el promedio hacia arriba.`,
        `El ${pct(v.filter(x => x < 100).length / v.length * 100)} de los pedidos vende menos de $100.`,
      ]);
    }

    function volumenValor(id, rows, key, labels, nombre) {
      const k = labels.length, n = new Array(k).fill(0), s = new Array(k).fill(0);
      for (const i of rows) { n[key[i]]++; s[key[i]] += D.Sales[i]; }
      const S = sum(s), pn = n.map(x => rows.length ? x / rows.length * 100 : 0), ps = s.map(x => S ? x / S * 100 : 0);
      const [tN, tS] = FIGS[id].data;
      Object.assign(tN, { y: pn, customdata: n.map(x => `${fmtInt(x)} pedidos`), marker: { color: pal.soft } });
      Object.assign(tS, { y: ps, customdata: s.map(money), marker: { color: pal.main } });
      draw(id);
      const idx = labels.map((_, j) => j).filter(j => n[j] > 0).sort((a, b) => n[b] - n[a]);
      if (!idx.length) return insight([]);
      if (idx.length === 1) return insight([`Con el filtro activo solo se ve <b>${labels[idx[0]]}</b> (${fmtInt(n[idx[0]])} pedidos).`]);
      const top = idx[0], gap = j => ps[j] - pn[j], val = argmax(idx, gap);
      return insight([
        `<b>${labels[top]}</b> concentra el ${pct(pn[top])} de los pedidos (${fmtInt(n[top])}), seguida de ` +
        `${lista(idx.slice(1).map(j => `${labels[j]} (${pct(pn[j])}, ${fmtInt(n[j])})`))}.`,
        Math.abs(gap(val)) >= 3 && val !== top
          ? `<b>${labels[val]}</b> logra el ${pct(ps[val])} de las ventas con el ${pct(pn[val])} de los pedidos: sus pedidos valen más.`
          : `En ${nombre.todas} el % de ventas es parecido al % de pedidos: ${nombre.el} cambia cuánto se vende, no el valor de cada pedido.`,
      ]);
    }

    function mapa(rows) {
      const k = D.states.length, n = new Array(k).fill(0);
      for (const i of rows) n[D.st[i]]++;
      Object.assign(FIGS['fig-map'].data[0], {
        z: n.map(x => x || null), colorscale: pal.scale,
        customdata: D.states.map((st, j) => [st, D.stateRegion[j]]),
      });
      draw('fig-map');
      const idx = D.states.map((_, j) => j).filter(j => n[j] > 0).sort((a, b) => n[b] - n[a]);
      if (!idx.length) return insight([]);
      const top = idx.slice(0, 3), pocos = idx.filter(j => n[j] < 10), ultimo = idx[idx.length - 1];
      return insight([
        `Los pedidos se concentran en pocos estados: ${lista(top.map(j => `<b>${D.states[j]}</b> (${fmtInt(n[j])})`))} ` +
        `suman el ${pct(sum(top.map(j => n[j])) / rows.length * 100)} del total.`,
        pocos.length && `En el otro extremo, ${pocos.length} estados tienen menos de 10 pedidos (p. ej. ${D.states[ultimo]} con ${fmtInt(n[ultimo])}).`,
      ]);
    }

    function cajas(id, rows, key, labels, nombre) {
      const ys = labels.map(() => []);
      for (const i of rows) ys[key[i]].push(D.Sales[i]);
      FIGS[id].data.forEach((t, j) => {
        const c = (pal.cats && pal.cats[labels[j]]) || pal.main;
        Object.assign(t, { y: ys[j], marker: { color: c }, line: { color: c, width: 1.5 }, fillcolor: pal.boxFill });
      });
      draw(id);
      const med = ys.map(median), mean = ys.map(a => (a.length ? sum(a) / a.length : null));
      const idx = labels.map((_, j) => j).filter(j => ys[j].length);
      if (!idx.length) return insight([]);
      if (idx.length === 1) return insight([`Con el filtro activo solo se ve <b>${labels[idx[0]]}</b> (mediana ${money(med[idx[0]])}).`]);
      const hi = argmax(idx, j => med[j]), lo = argmin(idx, j => med[j]), r = med[hi] / med[lo];
      const mx = argmax(idx, j => mean[j]);
      return insight([
        `Medianas: ${idx.map(j => `${labels[j]} ${money(med[j])}`).join(', ')}` +
        (r >= 2 ? `; <b>${labels[lo]}</b> queda muy por debajo.` : `; ${nombre.ningun} sobresale.`),
        `Medias: ${idx.map(j => `${labels[j]} ${money(mean[j])}`).join(', ')}; <b>${labels[mx]}</b> tiene la media más alta.`,
      ]);
    }

    function burbuja(rows) {
      const nc = D.cats.length, nr = D.regs.length, n = Array.from({ length: nc }, () => new Array(nr).fill(0));
      for (const i of rows) n[D.c[i]][D.r[i]]++;
      const max = Math.max(1, ...n.flat());
      FIGS['fig-bubble'].data.forEach((t, c) => Object.assign(t, {
        y: D.regs, x: D.regs.map(() => D.cats[c]), customdata: n[c],
        marker: { size: n[c], sizemode: 'area', sizeref: 2 * max / (45 * 45), sizemin: 2, color: pal.cats[D.cats[c]], line: { width: 1, color: pal.bg } },
      }));
      draw('fig-bubble');
      const tot = D.cats.map((_, c) => sum(n[c])), cs = D.cats.map((_, c) => c).filter(c => tot[c]);
      if (cs.length < 2) return insight(cs.length ? [`Con el filtro activo solo se ve <b>${D.cats[cs[0]]}</b>.`] : []);
      const rs = D.regs.map((_, r) => r).filter(r => cs.some(c => n[c][r]));
      const mayor = rs.map(r => argmax(cs, c => n[c][r])), menor = rs.map(r => argmin(cs, c => n[c][r]));
      const todas = (arr, c) => arr.every(x => x === c);
      if (rs.length === 1) {
        return insight([`En ${D.regs[rs[0]]}, <b>${D.cats[mayor[0]]}</b> tiene el círculo más grande y ` +
          `<b>${D.cats[menor[0]]}</b> el más pequeño.`]);
      }
      return insight([
        todas(mayor, mayor[0]) ? `<b>${D.cats[mayor[0]]}</b> tiene los círculos más grandes en todas las regiones.`
          : `El círculo más grande cambia por región: ${rs.map((r, j) => `${D.regs[r]} → ${D.cats[mayor[j]]}`).join(', ')}.`,
        todas(menor, menor[0]) && `<b>${D.cats[menor[0]]}</b> presenta los círculos más pequeños en todas las regiones.`,
      ]);
    }

    function bins(rows) {
      Object.assign(FIGS['fig-bins'].data[0], { x: rows.map(i => D.Discount[i]), y: rows.map(i => D.Sales[i]), colorscale: pal.seq });
      draw('fig-bins');
      if (rows.length < 3) return insight([]);
      const bajos = rows.filter(i => D.Discount[i] <= 0.2).length / rows.length * 100;
      const r = pearson(rows.map(i => D.Discount[i]), rows.map(i => D.Sales[i]));
      return insight([
        `El <b>${pct(bajos)}</b> de los pedidos tiene descuentos bajos (0.0-0.2).`,
        r !== null && `La correlación lineal entre Discount y Sales es ${Math.abs(r) < 0.1 ? 'prácticamente nula' : 'débil'} (${num(r)}).`,
      ]);
    }

    function correlacion(rows) {
      const cols = D.numeric.map(name => rows.map(i => D[name][i]));
      const z = cols.map(a => cols.map(b => { const r = pearson(a, b); return r === null ? null : Math.round(r * 100) / 100; }));
      Object.assign(FIGS['fig-corr'].data[0], { z, colorscale: pal.heat });
      draw('fig-corr');
      const otras = D.numeric.map((name, j) => ({ name, r: z[0][j] })).slice(1).filter(x => x.r !== null)
        .sort((a, b) => Math.abs(b.r) - Math.abs(a.r));
      if (!otras.length) return insight([]);
      const fuerza = r => (Math.abs(r) < 0.1 ? 'prácticamente nula'
        : `${r > 0 ? 'positiva' : 'negativa'} ${Math.abs(r) < 0.3 ? 'débil' : Math.abs(r) < 0.7 ? 'moderada' : 'fuerte'}`);
      return insight([`Sales y ${otras[0].name} tienen la correlación más alta del grupo (${num(otras[0].r)}, ${fuerza(otras[0].r)}). ` +
        otras.slice(1).map(x => `Sales y ${x.name}: ${fuerza(x.r)} (${num(x.r)}).`).join(' ')]);
    }

    function tendencia(f, rows) {
      const mIdx = D.months.map((_, j) => j).filter(j => f.year === 'all' || YEAR[j] === f.year);
      const pos = new Map(mIdx.map((j, p) => [j, p])), s = new Array(mIdx.length).fill(0);
      for (const i of rows) { const p = pos.get(D.m[i]); if (p !== undefined) s[p] += D.Sales[i]; }
      Object.assign(FIGS['fig-trend'].data[0], {
        x: mIdx.map(j => etiquetaMes(D.months[j])), y: s, customdata: s.map(money),
        line: { color: pal.soft, width: 1.5 }, marker: { color: pal.main, size: 7, line: { color: pal.bg, width: 1 } },
      });
      draw('fig-trend');
      const idx = s.map((_, p) => p).filter(p => s[p] > 0);
      if (!idx.length) return insight([]);
      const best = argmax(idx, p => s[p]), worst = argmin(idx, p => s[p]);
      const h1 = sum(idx.filter(p => +D.months[mIdx[p]].slice(5, 7) <= 6).map(p => s[p])), S = sum(s);
      return insight([
        `El mejor mes fue <b>${etiquetaMes(D.months[mIdx[best]])}</b> (${money(s[best])}) y el más bajo ` +
        `<b>${etiquetaMes(D.months[mIdx[worst]])}</b> (${money(s[worst])}).`,
        S && `El segundo semestre concentra el ${pct((S - h1) / S * 100, 0)} de las ventas${f.year === 'all' ? '' : ' del año'}.`,
      ]);
    }

    function tendenciaCategoria(f, rows) {
      const mIdx = D.months.map((_, j) => j).filter(j => f.year === 'all' || YEAR[j] === f.year);
      const pos = new Map(mIdx.map((j, p) => [j, p])), s = D.cats.map(() => new Array(mIdx.length).fill(0));
      for (const i of rows) { const p = pos.get(D.m[i]); if (p !== undefined) s[D.c[i]][p] += D.Sales[i]; }
      FIGS['fig-trend-cat'].data.forEach((t, c) => Object.assign(t, {
        x: mIdx.map(j => etiquetaMes(D.months[j])), y: s[c], line: { color: pal.cats[D.cats[c]], width: 2 },
      }));
      draw('fig-trend-cat');
      const cs = D.cats.map((_, c) => c).filter(c => sum(s[c]) > 0);
      if (cs.length < 2) return insight(cs.length ? [`Con el filtro activo solo se ve <b>${D.cats[cs[0]]}</b>.`] : []);
      const meses = mIdx.map((_, p) => p).filter(p => cs.some(c => s[c][p] > 0));
      const arriba = cs.map(c => meses.filter(p => argmax(cs, k => s[k][p]) === c).length);
      const lider = cs[argmax(cs.map((_, j) => j), j => arriba[j])];
      return insight([
        `<b>${D.cats[lider]}</b> vende más que las otras categorías en ${arriba[cs.indexOf(lider)]} de ${meses.length} meses.`,
      ]);
    }

    // ---------------------------------------------------------------- todo junto
    const CAT = { el: 'la categoría', todas: 'todas las categorías', ningun: 'ninguna categoría' };
    const REG = { el: 'la región', todas: 'todas las regiones', ningun: 'ninguna región' };
    const SEG = { el: 'el segmento', todas: 'todos los segmentos', ningun: 'ningún segmento' };

    function update(f) {
      const rows = rowsFor(f, f.year);
      setHTML('kpis', kpis(f, rows));
      if (!root.Plotly) return;
      const sinFiltros = f.cat < 0 && f.reg < 0 && f.seg < 0 && f.year === 'all';
      const ins = (id, calculado) => setHTML(`ins-${id}`,
        sinFiltros && D.eda[id] ? insight(D.eda[id].map(escape)) : calculado);
      ins('fig-hist', hist(rows));
      ins('fig-vv-cat', volumenValor('fig-vv-cat', rows, D.c, D.cats, CAT));
      ins('fig-vv-reg', volumenValor('fig-vv-reg', rows, D.r, D.regs, REG));
      ins('fig-vv-seg', volumenValor('fig-vv-seg', rows, D.s, D.segs, SEG));
      ins('fig-box-cat', cajas('fig-box-cat', rows, D.c, D.cats, CAT));
      ins('fig-box-reg', cajas('fig-box-reg', rows, D.r, D.regs, REG));
      ins('fig-box-seg', cajas('fig-box-seg', rows, D.s, D.segs, SEG));
      ins('fig-bubble', burbuja(rows));
      ins('fig-bins', bins(rows));
      ins('fig-corr', correlacion(rows));
      ins('fig-map', mapa(rows));
      ins('fig-trend', tendencia(f, rows));
      ins('fig-trend-cat', tendenciaCategoria(f, rows));
    }

    const q = D.quality;
    setHTML('calidad', `<b>Calidad de datos:</b> ${fmtInt(q.filas)} pedidos · ${fmtInt(q.faltantes)} faltantes · ` +
      `${fmtInt(q.duplicados)} duplicados · ${fmtInt(q.atipicos_sales)} atípicos en Sales y ${fmtInt(q.atipicos_profit)} en Profit (IQR), ` +
      'conservados en el análisis por ser pedidos reales.');

    return { update, years: YEARS };
  }

  root.SuperstoreDashboard = { create };
})(window);
