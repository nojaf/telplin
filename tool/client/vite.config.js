import { defineConfig } from 'vite'
import react from '@vitejs/plugin-react'
import fable from "vite-plugin-fable"
import Inspect from "vite-plugin-inspect"

// https://vitejs.dev/config/
export default defineConfig({
  plugins: [ Inspect(), fable(), react({ jsxRuntime: 'classic', include: /\.(jsx|fs)$/ }) ],
  base: '/telplin/'
})
