import{t as p,d as _}from"../chunks/transform.976a8eed.js";import{s as m,c as h,u as v,g as y,d as $}from"../chunks/scheduler.8f5c3a71.js";import{S as g,i as j,g as c,h as d,j as f,f as i,k as b,a as S,x as T,d as k,t as w}from"../chunks/index.bdc65bbe.js";/* empty css                    */const A=!0,E=!0,I="always";async function M({fetch:o}){const a="/data",[n,s]=await Promise.all([o(`${a}/UkraineTopo.json`).then(t=>t.json()),o(`${a}/data.json`).then(t=>t.json())]),l=n;return p.set(l),_.set(s),{data0:l,data3:_}}const D=Object.freeze(Object.defineProperty({__proto__:null,load:M,prerender:A,ssr:E,trailingSlash:I},Symbol.toStringTag,{value:"Module"}));function O(o){let a,n,s;const l=o[1].default,t=h(l,o,o[0],null);return{c(){a=c("div"),n=c("main"),t&&t.c(),this.h()},l(e){a=d(e,"DIV",{class:!0});var r=f(a);n=d(r,"MAIN",{});var u=f(n);t&&t.l(u),u.forEach(i),r.forEach(i),this.h()},h(){b(a,"class","app svelte-1o7dhv4")},m(e,r){S(e,a,r),T(a,n),t&&t.m(n,null),s=!0},p(e,[r]){t&&t.p&&(!s||r&1)&&v(t,l,e,e[0],s?$(l,e[0],r,null):y(e[0]),null)},i(e){s||(k(t,e),s=!0)},o(e){w(t,e),s=!1},d(e){e&&i(a),t&&t.d(e)}}}function P(o,a,n){let{$$slots:s={},$$scope:l}=a;return o.$$set=t=>{"$$scope"in t&&n(0,l=t.$$scope)},[l,s]}class L extends g{constructor(a){super(),j(this,a,P,O,m,{})}}export{L as component,D as universal};
