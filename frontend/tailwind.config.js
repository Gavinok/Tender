/** @type {import('tailwindcss').Config} */
module.exports = {
  // Class names live in the compiled PureScript bundle
  content: ["./assets/**/*.{html,js}"],
  theme: {
    extend: {
      colors: {
        tomato: {
          50: "#fff4f1",
          100: "#ffe4dc",
          200: "#ffc8b8",
          300: "#ffa089",
          400: "#ff7a5c",
          500: "#f4583a",
          600: "#dc3f22",
          700: "#b8301a",
          800: "#93291a",
          900: "#78261b",
        },
        cream: "#fff8f1",
        ink: "#2b1b17",
        butter: "#ffc94d",
      },
      fontFamily: {
        // Rounded system fonts where available (SF Pro Rounded on Apple devices)
        sans: ["ui-rounded", "Nunito", "Quicksand", "system-ui", "sans-serif"],
      },
      boxShadow: {
        card: "0 18px 40px -12px rgba(120, 38, 27, 0.35)",
        pop: "0 6px 0 0 rgba(120, 38, 27, 0.25)",
      },
      keyframes: {
        "card-in": {
          from: { opacity: "0", transform: "translateY(24px) scale(0.96)" },
          to: { opacity: "1", transform: "none" },
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
        pop: "pop 0.5s cubic-bezier(0.2, 0.9, 0.3, 1.3) both",
        shimmer: "shimmer 1.6s linear infinite",
        confetti: "confetti 2.8s linear both",
      },
    },
  },
  plugins: [],
};
