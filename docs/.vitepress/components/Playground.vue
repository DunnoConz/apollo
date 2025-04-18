<template>
  <div class="playground">
    <div class="editor-container">
      <MonacoEditor
        v-model="code"
        language="racket"
        theme="vs-dark"
        :options="editorOptions"
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
import { ref } from 'vue'
import MonacoEditor from './MonacoEditor.vue'

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
  minimap: { enabled: false },
  fontSize: 14,
  lineNumbers: 'on',
  roundedSelection: false,
  scrollBeyondLastLine: false,
  readOnly: false,
  automaticLayout: true
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

<style>
.playground {
  display: flex;
  flex-direction: column;
  gap: 1rem;
  padding: 1rem;
  background: var(--vp-c-bg);
  border-radius: 8px;
  border: 1px solid var(--vp-c-divider);
}

.editor-container {
  height: 300px;
  border: 1px solid var(--vp-c-divider);
  border-radius: 4px;
  overflow: hidden;
}

.controls {
  display: flex;
  gap: 0.5rem;
}

.run-button, .reset-button {
  padding: 0.5rem 1rem;
  border-radius: 4px;
  border: none;
  cursor: pointer;
  font-weight: 500;
}

.run-button {
  background: var(--vp-c-brand);
  color: white;
}

.run-button:disabled {
  opacity: 0.7;
  cursor: not-allowed;
}

.reset-button {
  background: var(--vp-c-gray-2);
  color: var(--vp-c-text-1);
}

.tabs {
  display: flex;
  gap: 0.5rem;
  margin-bottom: 0.5rem;
}

.tabs button {
  padding: 0.25rem 0.5rem;
  border: none;
  background: none;
  cursor: pointer;
  color: var(--vp-c-text-2);
}

.tabs button.active {
  color: var(--vp-c-brand);
  border-bottom: 2px solid var(--vp-c-brand);
}

.output-container {
  border: 1px solid var(--vp-c-divider);
  border-radius: 4px;
  padding: 1rem;
  min-height: 100px;
}

.output-content {
  font-family: monospace;
  white-space: pre-wrap;
}

.error {
  color: var(--vp-c-red);
  font-family: monospace;
  white-space: pre-wrap;
}
</style> 