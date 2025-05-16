<script lang="ts">
	import type { PageProps } from './$types';
	import FileList from '$lib/FileList.svelte';
	import FileUpload from '$lib/FileUpload.svelte';
	let { data }: PageProps = $props();
	let statusClass = data.success ? 'status-success' : 'status-failure';
</script>

<FileUpload />
<div class="status-message {statusClass}">
	{#if data.message}
		<p>{data.message}</p>
	{/if}
</div>
<FileList filelist={data.index} />

<style>
	div.status-message {
		display: none;

		border: 1px solid #5b5bbb !important;
		border-radius: 0.25rem !important;
		padding-left: 1rem;
		padding-right: 1rem;
	}

	div.status-success {
		background-color: #eaffe4;
	}

	div.status-failure {
		background-color: #f2cccc;
	}

	div.status-message:has(p) {
		display: block;
	}
</style>
