<script lang="ts">
	import '@fontsource/source-code-pro';

	const props = $props();
	const filelist = props.filelist;

	const ambiguate = (time: string) => {
		return new Date(time).toLocaleString(undefined, { timeZoneName: 'short' });
	};
</script>

<ul class="file-index">
	{#each filelist as file}
		<li>
			<a href="/api/files/by-uuid/{file.id}/{file.name}" class="file-entry">
				<span class="left">{file.name}</span>
				<span class="right">{ambiguate(file.creation_date)}</span>
			</a>
		</li>
	{/each}
</ul>

<style>
	ul.file-index {
		padding: 0;
	}

	ul.file-index > li {
		list-style-type: none;
	}

	a.file-entry {
		display: grid;
		grid-template-columns: 1fr auto;
		width: 100%;
		text-decoration: none;
		color: inherit;

		font-size: 14pt;
		column-gap: 8px;
	}

	a.file-entry > .right {
		color: #21689b;
		font-family: 'Source Code Pro', 'monospace';
	}

	a.file-entry > .left {
		text-wrap: nowrap;
		overflow: scroll;
		scrollbar-width: thin;
	}

	a.file-entry:hover {
		text-decoration: underline;
	}
</style>
