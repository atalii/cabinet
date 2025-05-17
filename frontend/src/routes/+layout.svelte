<script>
	import '@fontsource/source-sans-pro';
	import FileUpload from '$lib/FileUpload.svelte';
</script>

<div class="sidebar"></div>

<main>
	<div class="content">
		<slot />
	</div>
	<div class="sidebar">
		<FileUpload />
	</div>
</main>

<style>
	:root {
		font-family: 'Source Sans Pro', sans-serif;
	}

	:global {
		body {
			margin: 0;
		}
	}

	main {
		display: flex;
	}

	@media (max-width: 860px) {
		main {
			flex-direction: column-reverse;
		}

		main > div.sidebar {
			height: auto;
			border-bottom: 1px solid #8f8f8f;
		}

		main > div.content {
			width: calc(100vw - 2rem);
		}
	}
	@media not (max-width: 860px) {
		main > div.sidebar {
			width: 30%;
			height: 100vh;
			border-left: 1px solid #8f8f8f;
		}

		main > div.content {
			width: 8.5in;
		}
	}

	main > div.sidebar {
		padding: 1rem;
		background-color: #f8f8f8;
	}

	main > div.content {
		padding: 1rem;
		margin-left: auto;
		margin-right: auto;
	}
</style>
