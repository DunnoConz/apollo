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
      // Consider adding links to top-level sections if desired
      // { text: 'Tutorials', link: '/tutorials/' },
      // { text: 'How-to Guides', link: '/how-to/' },
      // { text: 'Reference', link: '/reference/' },
      // { text: 'Explanation', link: '/explanation/' }
    ],

    sidebar: [
      {
        text: 'Tutorials',
        items: [
          { text: 'Installation', link: '/tutorials/installation' }, // Moved from guide
          // Add more tutorial links here
          { text: 'Index', link: '/tutorials/' } // Link to the section index
        ],
        collapsible: true, // Optional: Make section collapsible
        collapsed: true    // Optional: Start collapsed
      },
      {
        text: 'How-to Guides',
        items: [
          // Add how-to guide links here
          { text: 'Index', link: '/how-to/' }
        ],
        collapsible: true,
        collapsed: true
      },
      {
        text: 'Reference',
        items: [
          // Add reference links here
          { text: 'Index', link: '/reference/' }
        ],
        collapsible: true,
        collapsed: true
      },
      {
        text: 'Explanation',
        items: [
          { text: 'Introduction', link: '/explanation/introduction' }, // Moved from guide
          // Add explanation links here
          { text: 'Index', link: '/explanation/' }
        ],
        collapsible: true,
        collapsed: true
      }
    ],

    socialLinks: [
      { icon: 'github', link: 'https://github.com/DunnoConz/apollo' } // Use actual repo URL
    ]
  }
}) 