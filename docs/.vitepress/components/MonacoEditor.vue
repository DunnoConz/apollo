#lang racket/base

<template>
  <div ref="editorContainer" class="monaco-editor"></div>
</template>

<script setup>
import { ref, onMounted, onBeforeUnmount, watch } from 'vue'
import * as monaco from 'monaco-editor'

const props = defineProps({
  modelValue: {
    type: String,
    default: ''
  },
  language: {
    type: String,
    default: 'racket'
  },
  theme: {
    type: String,
    default: 'vs-dark'
  },
  options: {
    type: Object,
    default: () => ({})
  }
})

const emit = defineEmits(['update:modelValue', 'editor-mounted'])

const editorContainer = ref(null)
let editor = null

// Register Racket language if not already registered
if (!monaco.languages.getLanguages().some(lang => lang.id === 'racket')) {
  monaco.languages.register({ id: 'racket' })
  monaco.languages.setMonarchTokensProvider('racket', {
    defaultToken: 'invalid',
    tokenizer: {
      root: [
        [/\(/, { token: 'delimiter.parenthesis.open' }],
        [/\)/, { token: 'delimiter.parenthesis.close' }],
        [/\[/, { token: 'delimiter.square.open' }],
        [/\]/, { token: 'delimiter.square.close' }],
        [/#[tf]/, 'constant.boolean'],
        [/#\\\w+/, 'constant.character'],
        [/#\d+/, 'constant.numeric'],
        [/"[^"]*"/, 'string'],
        [/'[^']*'/, 'string'],
        [/\d+/, 'constant.numeric'],
        [/[a-zA-Z_][a-zA-Z0-9_\-!?]*/, 'identifier'],
        [/[+\-*/=<>!&|]/, 'operator'],
        [/\s+/, 'white']
      ]
    }
  })
}

onMounted(() => {
  editor = monaco.editor.create(editorContainer.value, {
    value: props.modelValue,
    language: props.language,
    theme: props.theme,
    automaticLayout: true,
    minimap: { enabled: false },
    scrollBeyondLastLine: false,
    ...props.options
  })

  editor.onDidChangeModelContent(() => {
    const value = editor.getValue()
    emit('update:modelValue', value)
  })

  emit('editor-mounted', editor)
})

onBeforeUnmount(() => {
  if (editor) {
    editor.dispose()
  }
})

watch(() => props.modelValue, (newValue) => {
  if (editor && newValue !== editor.getValue()) {
    editor.setValue(newValue)
  }
})

watch(() => props.language, (newLanguage) => {
  if (editor) {
    monaco.editor.setModelLanguage(editor.getModel(), newLanguage)
  }
})

watch(() => props.theme, (newTheme) => {
  if (editor) {
    monaco.editor.setTheme(newTheme)
  }
})
</script>

<style scoped>
.monaco-editor {
  width: 100%;
  height: 300px;
  border: 1px solid #ccc;
  border-radius: 4px;
}
</style> 