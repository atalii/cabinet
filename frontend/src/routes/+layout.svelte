<script lang="ts">
	import '@fontsource/source-sans-pro';
	import FileUpload from '$lib/FileUpload.svelte';
	import BackendMetadata from '$lib/BackendMetadata.svelte';

	const props = $props();
	const { backendMetadata } = props.data;
</script>

<div class="sidebar"></div>

<main>
	<div class="content">
		{@render props.children()}
	</div>
	<div class="sidebar">
		<FileUpload />
		<div>
			<p class="build-info">Built from {__VERSION__}.</p>
			<BackendMetadata data={backendMetadata} />
		</div>
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
			flex-direction: column;
			min-height: 100vh;
			justify-content: space-between;
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
			height: calc(100vh - 2rem);
			border-left: 1px solid #8f8f8f;
			justify-content: space-between;
		}

		main > div.content {
			width: 8.5in;
		}
	}

	main > div.sidebar {
		display: flex;
		flex-direction: column;
		padding: 1rem;
		background-color: #f8f8f8;
	}

	main > div.content {
		padding: 1rem;
		margin-left: auto;
		margin-right: auto;
	}

	.build-info {
		color: #444;
		text-align: right;
	}
</style>
