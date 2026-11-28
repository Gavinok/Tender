/** @type {import('tailwindcss').Config} */
module.exports = {
  // Class names live in the compiled PureScript bundle
  content: ["./assets/**/*.{html,js}"],
  theme: {
    extend: {
      // Phones with room to spare, by height rather than width: tall ones
      // preview the summary on the card and show the lobby's vibe
      // suggestions, and roomy ones show every suggestion at once, which
      // still leaves a QR code over 220px. Mice can't swipe a row sideways,
      // so they get every suggestion too.
      screens: {
        tall: { raw: "(min-height: 700px)" },
        roomy: { raw: "(min-height: 840px)" },
        mouse: { raw: "(hover: hover) and (pointer: fine)" },
      },
      colors: {
        brand: {
          50: "#eef2ff",
          100: "#dbe3ff",
          200: "#b8c7ff",
          300: "#8aa2ff",
          400: "#5576ff",
          500: "#2a4bff",
          600: "#1a35e0",
          700: "#1528b3",
          800: "#14238c",
          900: "#131f6e",
        },
        surface: "#ffe4ef",
        ink: "#14122b",
        star: "#ffd60a",
      },
      fontFamily: {
        sans: ["Bricolage Grotesque", "system-ui", "sans-serif"],
        // Headings, the wordmark and other big text. Only comes in one weight.
        display: ["Bagel Fat One", "Bricolage Grotesque", "sans-serif"],
      },
      // Hard, unblurred shadows
      boxShadow: {
        card: "0 10px 0 0 #14122b",
        pop: "0 5px 0 0 #14122b",
      },
      keyframes: {
        "card-in": {
          from: { opacity: "0", transform: "translateY(24px) scale(0.96)" },
          to: { opacity: "1", transform: "none" },
        },
        "sheet-up": {
          from: { transform: "translateY(100%)" },
          to: { transform: "none" },
        },
        "toast-in": {
          from: { transform: "translateY(-150%)" },
          to: { transform: "none" },
        },
        "toast-out": {
          from: { transform: "none", opacity: "1" },
          to: { transform: "translateY(-150%)", opacity: "0" },
        },
        pop: {
          "0%": { transform: "scale(0.6) rotate(-8deg)", opacity: "0" },
          "60%": { transform: "scale(1.08) rotate(-3deg)", opacity: "1" },
          "100%": { transform: "scale(1) rotate(-3deg)" },
        },
        shimmer: {
          from: { backgroundPosition: "-200% 0" },
          to: { backgroundPosition: "200% 0" },
        },
        confetti: {
          from: { transform: "translateY(-10vh) rotate(0deg)" },
          to: { transform: "translateY(110vh) rotate(720deg)" },
        },
      },
      animation: {
        "card-in": "card-in 0.35s cubic-bezier(0.2, 0.9, 0.3, 1.2) both",
        "sheet-up": "sheet-up 0.25s cubic-bezier(0.2, 0.9, 0.3, 1) both",
        "toast-in": "toast-in 0.45s cubic-bezier(0.2, 0.9, 0.3, 1.25) both",
        "toast-out": "toast-out 0.25s cubic-bezier(0.5, 0, 0.75, 0) both",
        pop: "pop 0.5s cubic-bezier(0.2, 0.9, 0.3, 1.3) both",
        shimmer: "shimmer 1.6s linear infinite",
        confetti: "confetti 2.8s linear both",
      },
    },
  },
  plugins: [],
};
