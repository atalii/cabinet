import { sveltekit } from '@sveltejs/kit/vite';
import { defineConfig } from 'vite';
import { execSync } from 'node:child_process';

const version = process.env.CABINET_FRONTEND_VERSION;
const backend_port = process.env.CABINET_BACKEND_PORT;

export default defineConfig({
	plugins: [sveltekit()],
	server: {
		proxy: {
			'/api': {
				target: `http://localhost:${backend_port}`,
				changeOrigin: true,
				rewrite: (path) => path.replace(/^\/api/, '')
			}
		}
	},
	define: {
		__VERSION__: JSON.stringify(version)
	}
});
