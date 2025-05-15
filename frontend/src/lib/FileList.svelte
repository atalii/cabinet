<script>
	const props = $props();
	const filelist = props.filelist;
</script>

<ul class="file-index">
	{#each filelist as file}
		<li>
			<a href="/api/files/by-uuid/{file.id}/{file.name}" class="file-entry">
				<span class="left">{file.name}</span>
				<span class="right">{file.creation_date}</span>
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
		display: flex;
		width: 100%;
		text-decoration: none;
		justify-content: space-between;
		color: inherit;

		font-size: 14pt;
	}

	a.file-entry > .right {
		color: #5b5bbb;
	}

	a.file-entry > .left {
		/* By default, a flex child has a min-width of auto, which
		 * means that overly long children can break out of the
		 * container. */
		min-width: 0;
		overflow: scroll;
		scrollbar-width: thin;
	}

	a.file-entry:hover {
		text-decoration: underline;
	}
</style>
