<template>
  <div ref="editorContainer" class="monaco-editor"></div>
</template>

<script setup>
import { ref, onMounted, onBeforeUnmount, watch } from 'vue'
import { loader } from '@monaco-editor/loader'

const props = defineProps({
  modelValue: {
    type: String,
    default: ''
  },
  language: {
    type: String,
    default: 'javascript'
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

const emit = defineEmits(['update:modelValue', 'editorDidMount'])

const editorContainer = ref(null)
let editor = null

onMounted(async () => {
  const monaco = await loader.init()
  
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

  // Create editor instance
  editor = monaco.editor.create(editorContainer.value, {
    value: props.modelValue,
    language: props.language,
    theme: props.theme,
    ...props.options
  })

  // Set up model value sync
  editor.onDidChangeModelContent(() => {
    emit('update:modelValue', editor.getValue())
  })

  // Emit editor instance
  emit('editorDidMount', editor)
})

onBeforeUnmount(() => {
  if (editor) {
    editor.dispose()
  }
})

// Watch for prop changes
watch(() => props.modelValue, (newValue) => {
  if (editor && newValue !== editor.getValue()) {
    editor.setValue(newValue)
  }
})

watch(() => props.language, (newValue) => {
  if (editor) {
    monaco.editor.setModelLanguage(editor.getModel(), newValue)
  }
})

watch(() => props.theme, (newValue) => {
  if (editor) {
    monaco.editor.setTheme(newValue)
  }
})

watch(() => props.options, (newValue) => {
  if (editor) {
    editor.updateOptions(newValue)
  }
}, { deep: true })
</script>

<style scoped>
.monaco-editor {
  height: 100%;
  width: 100%;
}
</style> 