import{_ as i,c as a,o as e,ae as t}from"./chunks/framework.cCdmFsMI.js";const c=JSON.parse('{"title":"Debugging Apollo Projects","description":"","frontmatter":{},"headers":[],"relativePath":"how-to/debugging.md","filePath":"how-to/debugging.md"}'),n={name:"how-to/debugging.md"};function l(p,s,h,r,o,k){return e(),a("div",null,s[0]||(s[0]=[t(`<h1 id="debugging-apollo-projects" tabindex="-1">Debugging Apollo Projects <a class="header-anchor" href="#debugging-apollo-projects" aria-label="Permalink to &quot;Debugging Apollo Projects&quot;">​</a></h1><p>This guide explains how to debug issues in your Apollo projects.</p><h2 id="common-issues-and-solutions" tabindex="-1">Common Issues and Solutions <a class="header-anchor" href="#common-issues-and-solutions" aria-label="Permalink to &quot;Common Issues and Solutions&quot;">​</a></h2><h3 id="_1-compilation-errors" tabindex="-1">1. Compilation Errors <a class="header-anchor" href="#_1-compilation-errors" aria-label="Permalink to &quot;1. Compilation Errors&quot;">​</a></h3><p>If you get compilation errors:</p><div class="language-bash vp-adaptive-theme"><button title="Copy Code" class="copy"></button><span class="lang">bash</span><pre class="shiki shiki-themes github-light github-dark vp-code" tabindex="0"><code><span class="line"><span style="--shiki-light:#6A737D;--shiki-dark:#6A737D;"># Enable verbose output</span></span>
<span class="line"><span style="--shiki-light:#6F42C1;--shiki-dark:#B392F0;">apollo</span><span style="--shiki-light:#032F62;--shiki-dark:#9ECBFF;"> compile</span><span style="--shiki-light:#032F62;--shiki-dark:#9ECBFF;"> program.rkt</span><span style="--shiki-light:#005CC5;--shiki-dark:#79B8FF;"> -v</span></span>
<span class="line"></span>
<span class="line"><span style="--shiki-light:#6A737D;--shiki-dark:#6A737D;"># Check type errors specifically</span></span>
<span class="line"><span style="--shiki-light:#6F42C1;--shiki-dark:#B392F0;">apollo</span><span style="--shiki-light:#032F62;--shiki-dark:#9ECBFF;"> check</span><span style="--shiki-light:#032F62;--shiki-dark:#9ECBFF;"> program.rkt</span><span style="--shiki-light:#005CC5;--shiki-dark:#79B8FF;"> --strict</span></span></code></pre></div><h3 id="_2-runtime-errors-in-generated-code" tabindex="-1">2. Runtime Errors in Generated Code <a class="header-anchor" href="#_2-runtime-errors-in-generated-code" aria-label="Permalink to &quot;2. Runtime Errors in Generated Code&quot;">​</a></h3><p>For runtime errors in the generated Luau code:</p><ol><li><p>Enable source maps:</p><div class="language-bash vp-adaptive-theme"><button title="Copy Code" class="copy"></button><span class="lang">bash</span><pre class="shiki shiki-themes github-light github-dark vp-code" tabindex="0"><code><span class="line"><span style="--shiki-light:#6F42C1;--shiki-dark:#B392F0;">apollo</span><span style="--shiki-light:#032F62;--shiki-dark:#9ECBFF;"> compile</span><span style="--shiki-light:#032F62;--shiki-dark:#9ECBFF;"> program.rkt</span><span style="--shiki-light:#005CC5;--shiki-dark:#79B8FF;"> --source-maps</span></span></code></pre></div></li><li><p>Use the debug configuration:</p><div class="language-json vp-adaptive-theme"><button title="Copy Code" class="copy"></button><span class="lang">json</span><pre class="shiki shiki-themes github-light github-dark vp-code" tabindex="0"><code><span class="line"><span style="--shiki-light:#24292E;--shiki-dark:#E1E4E8;">{</span></span>
<span class="line"><span style="--shiki-light:#005CC5;--shiki-dark:#79B8FF;">  &quot;debug&quot;</span><span style="--shiki-light:#24292E;--shiki-dark:#E1E4E8;">: </span><span style="--shiki-light:#005CC5;--shiki-dark:#79B8FF;">true</span><span style="--shiki-light:#24292E;--shiki-dark:#E1E4E8;">,</span></span>
<span class="line"><span style="--shiki-light:#005CC5;--shiki-dark:#79B8FF;">  &quot;sourceMaps&quot;</span><span style="--shiki-light:#24292E;--shiki-dark:#E1E4E8;">: </span><span style="--shiki-light:#005CC5;--shiki-dark:#79B8FF;">true</span></span>
<span class="line"><span style="--shiki-light:#24292E;--shiki-dark:#E1E4E8;">}</span></span></code></pre></div></li></ol><h3 id="_3-type-mismatches" tabindex="-1">3. Type Mismatches <a class="header-anchor" href="#_3-type-mismatches" aria-label="Permalink to &quot;3. Type Mismatches&quot;">​</a></h3><p>For type-related issues:</p><ol><li><p>Add explicit type annotations:</p><div class="language-racket vp-adaptive-theme"><button title="Copy Code" class="copy"></button><span class="lang">racket</span><pre class="shiki shiki-themes github-light github-dark vp-code" tabindex="0"><code><span class="line"><span style="--shiki-light:#D73A49;--shiki-dark:#F97583;">#lang</span><span style="--shiki-light:#24292E;--shiki-dark:#E1E4E8;"> typed/racket</span></span>
<span class="line"><span style="--shiki-light:#24292E;--shiki-dark:#E1E4E8;">(: my-function (</span><span style="--shiki-light:#005CC5;--shiki-dark:#79B8FF;">-&gt;</span><span style="--shiki-light:#24292E;--shiki-dark:#E1E4E8;"> Number String))</span></span></code></pre></div></li><li><p>Use contracts for runtime checking:</p><div class="language-racket vp-adaptive-theme"><button title="Copy Code" class="copy"></button><span class="lang">racket</span><pre class="shiki shiki-themes github-light github-dark vp-code" tabindex="0"><code><span class="line"><span style="--shiki-light:#24292E;--shiki-dark:#E1E4E8;">(</span><span style="--shiki-light:#005CC5;--shiki-dark:#79B8FF;">require</span><span style="--shiki-light:#24292E;--shiki-dark:#E1E4E8;"> racket/contract)</span></span>
<span class="line"><span style="--shiki-light:#24292E;--shiki-dark:#E1E4E8;">(</span><span style="--shiki-light:#005CC5;--shiki-dark:#79B8FF;">define/contract</span><span style="--shiki-light:#24292E;--shiki-dark:#E1E4E8;"> (my-function x)</span></span>
<span class="line"><span style="--shiki-light:#24292E;--shiki-dark:#E1E4E8;">  (</span><span style="--shiki-light:#005CC5;--shiki-dark:#79B8FF;">-&gt;</span><span style="--shiki-light:#005CC5;--shiki-dark:#79B8FF;"> number?</span><span style="--shiki-light:#005CC5;--shiki-dark:#79B8FF;"> string?</span><span style="--shiki-light:#24292E;--shiki-dark:#E1E4E8;">)</span></span>
<span class="line"><span style="--shiki-light:#005CC5;--shiki-dark:#79B8FF;">  ...</span><span style="--shiki-light:#24292E;--shiki-dark:#E1E4E8;">)</span></span></code></pre></div></li></ol><h2 id="debugging-tools" tabindex="-1">Debugging Tools <a class="header-anchor" href="#debugging-tools" aria-label="Permalink to &quot;Debugging Tools&quot;">​</a></h2><h3 id="_1-logging" tabindex="-1">1. Logging <a class="header-anchor" href="#_1-logging" aria-label="Permalink to &quot;1. Logging&quot;">​</a></h3><p>Add debug prints in Racket:</p><div class="language-racket vp-adaptive-theme"><button title="Copy Code" class="copy"></button><span class="lang">racket</span><pre class="shiki shiki-themes github-light github-dark vp-code" tabindex="0"><code><span class="line"><span style="--shiki-light:#24292E;--shiki-dark:#E1E4E8;">(</span><span style="--shiki-light:#D73A49;--shiki-dark:#F97583;">define</span><span style="--shiki-light:#24292E;--shiki-dark:#E1E4E8;"> (</span><span style="--shiki-light:#6F42C1;--shiki-dark:#B392F0;">debug-print</span><span style="--shiki-light:#E36209;--shiki-dark:#FFAB70;"> msg</span><span style="--shiki-light:#24292E;--shiki-dark:#E1E4E8;">)</span></span>
<span class="line"><span style="--shiki-light:#24292E;--shiki-dark:#E1E4E8;">  (</span><span style="--shiki-light:#005CC5;--shiki-dark:#79B8FF;">printf</span><span style="--shiki-light:#032F62;--shiki-dark:#9ECBFF;"> &quot;[DEBUG] </span><span style="--shiki-light:#005CC5;--shiki-dark:#79B8FF;">~a\\n</span><span style="--shiki-light:#032F62;--shiki-dark:#9ECBFF;">&quot;</span><span style="--shiki-light:#24292E;--shiki-dark:#E1E4E8;"> msg))</span></span></code></pre></div><h3 id="_2-type-checking" tabindex="-1">2. Type Checking <a class="header-anchor" href="#_2-type-checking" aria-label="Permalink to &quot;2. Type Checking&quot;">​</a></h3><p>Use the type checker in strict mode:</p><div class="language-bash vp-adaptive-theme"><button title="Copy Code" class="copy"></button><span class="lang">bash</span><pre class="shiki shiki-themes github-light github-dark vp-code" tabindex="0"><code><span class="line"><span style="--shiki-light:#6F42C1;--shiki-dark:#B392F0;">apollo</span><span style="--shiki-light:#032F62;--shiki-dark:#9ECBFF;"> check</span><span style="--shiki-light:#032F62;--shiki-dark:#9ECBFF;"> program.rkt</span><span style="--shiki-light:#005CC5;--shiki-dark:#79B8FF;"> --strict</span></span></code></pre></div><h3 id="_3-source-maps" tabindex="-1">3. Source Maps <a class="header-anchor" href="#_3-source-maps" aria-label="Permalink to &quot;3. Source Maps&quot;">​</a></h3><p>Generate and use source maps to trace issues back to Racket code.</p><h2 id="best-practices" tabindex="-1">Best Practices <a class="header-anchor" href="#best-practices" aria-label="Permalink to &quot;Best Practices&quot;">​</a></h2><ol><li>Start with type checking</li><li>Use verbose compilation output</li><li>Add strategic debug prints</li><li>Keep Racket code simple when possible</li><li>Test generated Luau code separately</li></ol><h2 id="see-also" tabindex="-1">See Also <a class="header-anchor" href="#see-also" aria-label="Permalink to &quot;See Also&quot;">​</a></h2><ul><li><a href="./../reference/errors.html">Error Messages Reference</a></li><li><a href="./../explanation/type-system.html">Type System</a></li></ul>`,25)]))}const g=i(n,[["render",l]]);export{c as __pageData,g as default};
