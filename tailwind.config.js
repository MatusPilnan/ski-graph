const colors = require('tailwindcss/colors')

module.exports = {
  content: ['./public/*.html', './**/*.elm'],
  theme: {
    extend: {
      colors: {
        'primary': colors.blue["400"],
        'secondary': colors.yellow["300"]
      }
    }
  }
}
