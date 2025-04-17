import { defineConfig } from 'vitepress'

// https://vitepress.dev/reference/site-config
export default defineConfig({
  title: "Apollo Compiler",
  description: "Documentation for the Apollo Racket-to-Luau Compiler",
  base: '/apollo/', // Base path for GitHub Pages deployment
  themeConfig: {
    // https://vitepress.dev/reference/default-theme-config
    nav: [
      { text: 'Home', link: '/' },
      { text: 'Tutorials', link: '/tutorials/' },
      { text: 'How-to Guides', link: '/how-to/' },
      { text: 'Reference', link: '/reference/' },
      { text: 'Explanation', link: '/explanation/' }
    ],

    sidebar: [
      {
        text: 'Tutorials',
        items: [
          { text: 'Getting Started', link: '/tutorials/' },
          { text: 'Installation', link: '/tutorials/installation' },
          { text: 'First Project', link: '/tutorials/first-project' },
          { text: 'Advanced Features', link: '/tutorials/advanced' },
          { text: 'Advanced Macros', link: '/tutorials/advanced-macros' },
          { text: 'Examples', link: '/tutorials/examples' }
        ]
      },
      {
        text: 'How-to Guides',
        items: [
          { text: 'Overview', link: '/how-to/' },
          { text: 'Working with Roblox', link: '/how-to/roblox' },
          { text: 'Debugging', link: '/how-to/debugging' },
          { text: 'Using Macros', link: '/how-to/macros' }
        ]
      },
      {
        text: 'Reference',
        items: [
          { text: 'Overview', link: '/reference/' },
          { text: 'CLI Reference', link: '/reference/cli' },
          { text: 'Configuration', link: '/reference/config' },
          { text: 'Error Messages', link: '/reference/errors' },
          { text: 'Standard Library', link: '/reference/stdlib' },
          { text: 'Macros', link: '/reference/macros' }
        ]
      },
      {
        text: 'Explanation',
        items: [
          { text: 'Overview', link: '/explanation/' },
          { text: 'Introduction', link: '/explanation/introduction' },
          { text: 'Architecture', link: '/explanation/architecture' },
          { text: 'Type System', link: '/explanation/type-system' },
          { text: 'Compilation Process', link: '/explanation/compilation' }
        ]
      }
    ],

    socialLinks: [
      { icon: 'github', link: 'https://github.com/DunnoConz/apollo' } // Use actual repo URL
    ]
  }
}) 