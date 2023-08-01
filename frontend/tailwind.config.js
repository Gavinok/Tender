/** @type {import('tailwindcss').Config} */
// tailwind.config.js
module.exports = {
  experimental: {
      applyComplexClasses: true,
  },
  content: [
      "./assets/**/*.{html,js,ts,jsx,tsx,css}",
  ],
  theme: {
    extend: {},
  },
  plugins: [],
}
