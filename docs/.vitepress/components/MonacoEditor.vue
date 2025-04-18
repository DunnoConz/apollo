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
  
  // Only register Racket language if it hasn't been registered yet
  if (!monaco.languages.getLanguages().some(lang => lang.id === 'racket')) {
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
        'define', 'lambda', 'let', 'let*', 'letrec', 'if', 'cond', 'case',
        'and', 'or', 'begin', 'do', 'delay', 'force', 'promise',
        'quasiquote', 'unquote', 'unquote-splicing',
        'quote', 'syntax', 'syntax-case', 'syntax-rules',
        'set!', 'values', 'call-with-values',
        'dynamic-wind', 'parameterize', 'guard', 'raise',
        'with-handlers', 'call/cc', 'call-with-current-continuation',
        'error', 'display', 'newline', 'read', 'write',
        'load', 'require', 'provide', 'module', 'submod',
        'struct', 'match', 'match-let', 'match-let*', 'match-define',
        'for', 'for/list', 'for/vector', 'for/hash', 'for/hasheq',
        'for/and', 'for/or', 'for/first', 'for/last', 'for/fold',
        'for*/list', 'for*/vector', 'for*/hash', 'for*/hasheq',
        'for*/and', 'for*/or', 'for*/first', 'for*/last', 'for*/fold'
      ],

      operators: [
        '+', '-', '*', '/', '=', '<', '>', '<=', '>=', 'eq?', 'eqv?', 'equal?',
        'not', 'null?', 'pair?', 'list?', 'vector?', 'string?', 'symbol?',
        'number?', 'boolean?', 'procedure?', 'char?', 'port?', 'eof-object?',
        'cons', 'car', 'cdr', 'caar', 'cadr', 'cdar', 'cddr',
        'list', 'list*', 'append', 'reverse', 'length',
        'map', 'filter', 'foldl', 'foldr', 'for-each',
        'apply', 'call-with-current-continuation', 'call/cc'
      ],

      symbols: /[=><!~?:&|+\-*\/\^%]+/,
      escapes: /\\(?:[abfnrtv\\"']|x[0-9A-Fa-f]{1,4}|u[0-9A-Fa-f]{4}|U[0-9A-Fa-f]{8})/,

      tokenizer: {
        root: [
          [/\s+/, 'white'],
          [/(\(|\)|\[|\]|\{|\})/, '@brackets'],
          [/(#lang\s+)([a-zA-Z0-9\-]+)/, ['keyword', 'type.identifier']],
          [/(#;|#\|)/, 'comment.block'],
          [/(;.*$)/, 'comment.line'],
          [/(#t|#f)/, 'constant.boolean'],
          [/(#\\[a-zA-Z]+)/, 'constant.character'],
          [/(#x[0-9a-fA-F]+|#b[01]+|#o[0-7]+|\d+\.\d+|\d+)/, 'constant.numeric'],
          [/(#[0-9]+=)/, 'constant.other'],
          [/(#[0-9]+#)/, 'constant.other'],
          [/(['`])(\()/, ['operator', '@brackets']],
          [/(['`])([^'`\s\(\)\[\]\{\}]+)/, ['operator', 'string']],
          [/(['`])(\s)/, ['operator', 'white']],
          [/(,@)/, 'operator'],
          [/(,)/, 'operator'],
          [/(['`])/, 'operator'],
          [/[a-zA-Z_][a-zA-Z0-9_\-!?]*/, {
            cases: {
              '@keywords': 'keyword',
              '@operators': 'operator',
              '@default': 'identifier'
            }
          }],
          [/"([^"\\]|\\.)*$/, 'string.invalid'],
          [/"/, 'string', '@string']
        ],

        string: [
          [/[^\\"]+/, 'string'],
          [/@escapes/, 'string.escape'],
          [/\\./, 'string.escape.invalid'],
          [/"/, 'string', '@pop']
        ]
      }
    })
  }

  editor = monaco.editor.create(editorContainer.value, {
    value: props.modelValue,
    language: props.language,
    theme: props.theme,
    ...props.options
  })

  editor.onDidChangeModelContent(() => {
    emit('update:modelValue', editor.getValue())
  })

  emit('editorDidMount', editor)
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
</script>

<style scoped>
.monaco-editor {
  width: 100%;
  height: 100%;
  min-height: 300px;
}
</style> 