<template>
  <div ref="editorContainer" class="monaco-editor"></div>
</template>

<script setup>
import { ref, onMounted, onBeforeUnmount, watch } from 'vue'
import loader from '@monaco-editor/loader'

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

// Configure Monaco loader
loader.config({
  paths: {
    vs: 'https://cdn.jsdelivr.net/npm/monaco-editor@0.45.0/min/vs'
  }
})

onMounted(async () => {
  // Load Monaco
  const monaco = await loader.init()
  
  // Register Racket language
  monaco.languages.register({ id: 'racket' })
  monaco.languages.setMonarchTokensProvider('racket', {
    defaultToken: '',
    tokenizer: {
      root: [
        [/;.*$/, 'comment'],
        [/#lang\s+[a-zA-Z][a-zA-Z0-9\/_-]*/, 'keyword'],
        [/\(define\s+\(([^)]+)\)/, 'keyword'],
        [/\(define\s+[a-zA-Z][a-zA-Z0-9\/_-]*/, 'keyword'],
        [/\(lambda\s+/, 'keyword'],
        [/\(let\s+/, 'keyword'],
        [/\(let\*\s+/, 'keyword'],
        [/\(letrec\s+/, 'keyword'],
        [/\(if\s+/, 'keyword'],
        [/\(cond\s+/, 'keyword'],
        [/\(case\s+/, 'keyword'],
        [/\(and\s+/, 'keyword'],
        [/\(or\s+/, 'keyword'],
        [/\(not\s+/, 'keyword'],
        [/\(begin\s+/, 'keyword'],
        [/\(set!\s+/, 'keyword'],
        [/\(quote\s+/, 'keyword'],
        [/\(quasiquote\s+/, 'keyword'],
        [/\(unquote\s+/, 'keyword'],
        [/\(unquote-splicing\s+/, 'keyword'],
        [/\(module\s+/, 'keyword'],
        [/\(require\s+/, 'keyword'],
        [/\(provide\s+/, 'keyword'],
        [/\(struct\s+/, 'keyword'],
        [/\(class\s+/, 'keyword'],
        [/\(interface\s+/, 'keyword'],
        [/\(mixin\s+/, 'keyword'],
        [/\(trait\s+/, 'keyword'],
        [/\(match\s+/, 'keyword'],
        [/\(match-let\s+/, 'keyword'],
        [/\(match-let\*\s+/, 'keyword'],
        [/\(match-letrec\s+/, 'keyword'],
        [/\(for\s+/, 'keyword'],
        [/\(for\/list\s+/, 'keyword'],
        [/\(for\/vector\s+/, 'keyword'],
        [/\(for\/hash\s+/, 'keyword'],
        [/\(for\/sum\s+/, 'keyword'],
        [/\(for\/product\s+/, 'keyword'],
        [/\(for\/and\s+/, 'keyword'],
        [/\(for\/or\s+/, 'keyword'],
        [/\(for\/first\s+/, 'keyword'],
        [/\(for\/last\s+/, 'keyword'],
        [/\(for\/fold\s+/, 'keyword'],
        [/\(for\*\s+/, 'keyword'],
        [/\(for\*\/list\s+/, 'keyword'],
        [/\(for\*\/vector\s+/, 'keyword'],
        [/\(for\*\/hash\s+/, 'keyword'],
        [/\(for\*\/sum\s+/, 'keyword'],
        [/\(for\*\/product\s+/, 'keyword'],
        [/\(for\*\/and\s+/, 'keyword'],
        [/\(for\*\/or\s+/, 'keyword'],
        [/\(for\*\/first\s+/, 'keyword'],
        [/\(for\*\/last\s+/, 'keyword'],
        [/\(for\*\/fold\s+/, 'keyword'],
        [/\(/, 'delimiter.parenthesis'],
        [/\)/, 'delimiter.parenthesis'],
        [/\[/, 'delimiter.square'],
        [/\]/, 'delimiter.square'],
        [/\{/, 'delimiter.curly'],
        [/\}/, 'delimiter.curly'],
        [/#t/, 'constant.boolean'],
        [/#f/, 'constant.boolean'],
        [/#\\[a-zA-Z]+/, 'constant.character'],
        [/#\\x[0-9a-fA-F]+/, 'constant.character'],
        [/#\\[0-9]+/, 'constant.character'],
        [/"[^"]*"/, 'string'],
        [/'[^']*'/, 'string'],
        [/[0-9]+\.[0-9]+/, 'number.float'],
        [/[0-9]+/, 'number'],
        [/[a-zA-Z_][a-zA-Z0-9_\-!?]*/, 'identifier'],
        [/[+\-*/=<>!&|]/, 'operator'],
        [/\s+/, 'white']
      ]
    }
  })

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

<style>
.monaco-editor {
  width: 100%;
  height: 100%;
  min-height: 300px;
}
</style> 