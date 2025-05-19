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
			<details>
				<summary>
					<a href="/api/files/by-uuid/{file.id}/{file.name}" class="file-entry">
						<span class="left">{file.name}</span>
						<span class="right">{ambiguate(file.creation_date)}</span>
					</a>
				</summary>
				<form class="attrs" action="/api/set-attrs/by-uuid/{file.id}" method="post">
					<ul>
						<li>
							<input type="checkbox" checked={file.is_public ? true : null} name="public" />
							<label for="public">Public</label>
						</li>

						<li>
							<input type="checkbox" checked={file.is_sticky ? true : null} name="sticky" />
							<label for="sticky">Sticky</label>
						</li>
					</ul>
					<input type="submit" value="Update attributes." />
				</form>
			</details>
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

	summary {
		width: 100%;
		list-style: none;
	}

	summary::before {
		content: '→';
		display: inline-block;
		width: 2rem;
	}

	details[open] > summary::before {
		content: '↓';
	}

	form > ul > li {
		list-style: none;
	}

	a.file-entry {
		display: inline-grid;
		width: calc(100% - 2rem);

		grid-template-columns: 1fr auto;
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

	form.attrs {
		display: flex;
		align-items: flex-end;
		padding-top: 1rem;
		padding-bottom: 1rem;

		/* Line up with the content of the summary, after the
		 * ::before. */
		padding-left: 2rem;
	}

	form.attrs > ul {
		flex-grow: 1;
		padding-left: 0;
		padding-right: 0;
	}

	input[type='submit'] {
		font-family: inherit;
		font-size: inherit;
		padding: 10px;
		background: none;
		border: 1px solid #f6ae2d;
	}

	input[type='submit']:hover {
		cursor: pointer;
		background-color: #f0dfc2;
	}
</style>
