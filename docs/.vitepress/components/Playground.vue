<template>
  <div class="playground">
    <div class="editor-container">
      <MonacoEditor
        v-model="code"
        language="racket"
        theme="vs-dark"
        :options="editorOptions"
        @change="handleCodeChange"
        @editorDidMount="handleEditorDidMount"
      />
    </div>
    <div class="controls">
      <button @click="runCode" class="run-button" :disabled="isRunning">
        {{ isRunning ? 'Running...' : 'Run' }}
      </button>
      <button @click="resetCode" class="reset-button">Reset</button>
    </div>
    <div class="output-container">
      <div class="tabs">
        <button 
          v-for="tab in tabs" 
          :key="tab.id"
          :class="{ active: activeTab === tab.id }"
          @click="activeTab = tab.id"
        >
          {{ tab.label }}
        </button>
      </div>
      <div class="output-content">
        <pre v-if="activeTab === 'output' && output" class="output">{{ output }}</pre>
        <pre v-if="activeTab === 'luau' && luauOutput" class="output">{{ luauOutput }}</pre>
        <div v-if="error" class="error">{{ error }}</div>
      </div>
    </div>
  </div>
</template>

<script setup>
import { ref, onMounted } from 'vue'
import MonacoEditor from '@monaco-editor/vue3'
import { loader } from '@monaco-editor/loader'

// Initialize Monaco
loader.init().then(monaco => {
  // Register Racket language
  monaco.languages.register({ id: 'racket' })
  
  // Define Racket syntax highlighting
  monaco.languages.setMonarchTokensProvider('racket', {
    defaultToken: '',
    tokenPostfix: '.rkt',
    
    brackets: [
      { open: '(', close: ')', token: 'delimiter.parenthesis' },
      { open: '[', close: ']', token: 'delimiter.square' },
      { open: '{', close: '}', token: 'delimiter.curly' }
    ],

    keywords: [
      'define', 'lambda', 'if', 'else', 'cond', 'case', 'and', 'or',
      'let', 'let*', 'letrec', 'begin', 'do', 'delay', 'set!',
      'quote', 'quasiquote', 'unquote', 'unquote-splicing',
      'require', 'provide', 'module', 'struct'
    ],

    operators: [
      '+', '-', '*', '/', '=', '<', '>', '<=', '>=', 'eq?', 'eqv?', 'equal?',
      'not', 'and', 'or', 'cons', 'car', 'cdr', 'list', 'append', 'map', 'filter'
    ],

    symbols: /[=><!~?:&|+\-*\/\^%]+/,
    escapes: /\\(?:[abfnrtv\\"']|x[0-9A-Fa-f]{1,4}|u[0-9A-Fa-f]{4}|U[0-9A-Fa-f]{8})/,

    tokenizer: {
      root: [
        [/#lang\s+[a-zA-Z_][a-zA-Z0-9_]*/, 'keyword'],
        [/[a-zA-Z_][a-zA-Z0-9_!?]*/, {
          cases: {
            '@keywords': 'keyword',
            '@default': 'identifier'
          }
        }],
        { include: '@whitespace' },
        [/[()\[\]]/, '@brackets'],
        [/@symbols/, {
          cases: {
            '@operators': 'operator',
            '@default': ''
          }
        }],
        [/\d+/, 'number'],
        [/"/, 'string', '@string']
      ],

      whitespace: [
        [/\s+/, 'white']
      ],

      string: [
        [/[^\\"]+/, 'string'],
        [/@escapes/, 'string.escape'],
        [/\\./, 'string.escape.invalid'],
        [/"/, 'string', '@pop']
      ]
    }
  })
})

const code = ref(`#lang racket

;; Welcome to the Apollo Playground!
;; Try writing some Racket code that will be compiled to Luau.

(define (hello-world)
  (displayln "Hello, Roblox!"))

(hello-world)`)
const output = ref('')
const luauOutput = ref('')
const error = ref('')
const isRunning = ref(false)
const activeTab = ref('output')

const tabs = [
  { id: 'output', label: 'Output' },
  { id: 'luau', label: 'Luau' }
]

const editorOptions = {
  automaticLayout: true,
  minimap: { enabled: false },
  fontSize: 14,
  lineNumbers: 'on',
  roundedSelection: false,
  scrollBeyondLastLine: false,
  readOnly: false,
  theme: 'vs-dark',
  tabSize: 2,
  insertSpaces: true,
  wordWrap: 'on',
  folding: true,
  lineDecorationsWidth: 0,
  lineNumbersMinChars: 3
}

const handleEditorDidMount = (editor) => {
  // Add custom commands
  editor.addCommand(monaco.KeyMod.CtrlCmd | monaco.KeyCode.Enter, () => {
    runCode()
  })
}

const handleCodeChange = (value) => {
  code.value = value
}

const runCode = async () => {
  output.value = ''
  luauOutput.value = ''
  error.value = ''
  isRunning.value = true
  
  try {
    // Create a temporary file with the code
    const response = await fetch('/api/compile', {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
      },
      body: JSON.stringify({ code: code.value })
    })

    if (!response.ok) {
      throw new Error('Compilation failed')
    }

    const result = await response.json()
    output.value = result.output || 'No output'
    luauOutput.value = result.luau || 'No Luau output'
  } catch (e) {
    error.value = e.message
  } finally {
    isRunning.value = false
  }
}

const resetCode = () => {
  code.value = `#lang racket

;; Welcome to the Apollo Playground!
;; Try writing some Racket code that will be compiled to Luau.

(define (hello-world)
  (displayln "Hello, Roblox!"))

(hello-world)`
  output.value = ''
  luauOutput.value = ''
  error.value = ''
}
</script>

<style scoped>
.playground {
  display: flex;
  flex-direction: column;
  gap: 1rem;
  margin: 2rem 0;
  border: 1px solid var(--vp-c-divider);
  border-radius: 8px;
  overflow: hidden;
}

.editor-container {
  height: 400px;
  border-bottom: 1px solid var(--vp-c-divider);
}

.controls {
  display: flex;
  gap: 1rem;
  padding: 1rem;
  background: var(--vp-c-bg-soft);
}

.run-button, .reset-button {
  padding: 0.5rem 1rem;
  border-radius: 4px;
  border: none;
  cursor: pointer;
  font-weight: 500;
  transition: opacity 0.2s;
}

.run-button:disabled {
  opacity: 0.7;
  cursor: not-allowed;
}

.run-button {
  background: var(--vp-c-brand);
  color: white;
}

.reset-button {
  background: var(--vp-c-bg-alt);
  color: var(--vp-c-text-1);
}

.output-container {
  display: flex;
  flex-direction: column;
  background: var(--vp-c-bg-soft);
}

.tabs {
  display: flex;
  border-bottom: 1px solid var(--vp-c-divider);
}

.tabs button {
  padding: 0.5rem 1rem;
  background: none;
  border: none;
  cursor: pointer;
  color: var(--vp-c-text-2);
  transition: color 0.2s;
}

.tabs button.active {
  color: var(--vp-c-brand);
  border-bottom: 2px solid var(--vp-c-brand);
}

.output-content {
  padding: 1rem;
  min-height: 100px;
}

.output {
  margin: 0;
  white-space: pre-wrap;
  font-family: monospace;
}

.error {
  color: var(--vp-c-red);
  font-family: monospace;
  padding: 1rem;
  background: var(--vp-c-red-soft);
  border-radius: 4px;
}
</style> 