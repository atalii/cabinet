<script lang="ts">
	import '@fontsource/source-code-pro';

	interface File {
		is_public: string;
		id: string;
		name: string;
	}

	const props = $props();
	const filelist = props.filelist;

	const ambiguate = (time: string) => {
		return new Date(time).toLocaleString(undefined, { timeZoneName: 'short' });
	};

	const urlForFile = (file: File) => {
		return `/api/files/by-uuid/${file.is_public ? 'public/' : ''}${file.id}/${file.name}`;
	};

	type Dir = 'Up' | 'Down';
	type Key = 'Name' | 'Date';

	const sort = (dir: Dir, key: Key) => {
		const assets = document.getElementById('assets');
		if (!assets) return;
		[...assets.children]
			.sort((a, b) => {
				let values = dir === 'Up' ? [1, -1] : [-1, 1];
				let getter =
					key === 'Name'
						? (a: Element) => a.querySelector('details > summary > a > span.left')
						: (a: Element) => a.querySelector('details > summary > a > span.right');

				let acontent = getter(a)?.textContent;
				let bcontent = getter(b)?.textContent;

				if (!acontent) {
					return values[1];
				}

				if (!bcontent) {
					return values[0];
				}

				if (acontent > bcontent) {
					return values[0];
				} else if (bcontent > acontent) {
					return values[1];
				} else {
					return 0;
				}
			})
			.forEach((node) => assets.appendChild(node));
	};

	let filterString = $state('');
</script>

<div class="panel">
	<input type="text" placeholder="Filter list." bind:value={filterString} />
	<div class="sort-buttons">
		<span>
			Sort by name:
			<button onclick={(_) => sort('Up', 'Name')}>↑</button>
			<button onclick={(_) => sort('Down', 'Name')}>↓</button>
		</span>
		<span>
			Sort by date:
			<button onclick={(_) => sort('Up', 'Date')}>↑</button>
			<button onclick={(_) => sort('Down', 'Date')}>↓</button>
		</span>
	</div>
</div>

<ul class="file-index" id="assets">
	{#each filelist as file}
		<li class={file.name.includes(filterString) ? null : 'hidden'}>
			<details>
				<summary>
					<a href={urlForFile(file)} class="file-entry">
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
				<form class="mimeupdate" action="/api/set-mime-type/by-uuid/{file.id}" method="post">
					<div>
						<label for="type">MIME Type:</label>
						<input name="type" type="text" value={file.mime_type} />
					</div>
					<input type="submit" value="Update MIME type." />
				</form>
			</details>
		</li>
	{/each}
</ul>

<style>
	.hidden {
		display: none;
	}

	ul.file-index {
		padding: 0;
	}

	ul.file-index > li {
		list-style-type: none;
		padding: 1rem;
		border-bottom: 1px solid #a4a4a4;
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

		grid-template-columns: auto 1fr;

		text-decoration: none;
		color: inherit;

		font-size: 14pt;
		column-gap: 8px;
	}

	a.file-entry > .right {
		color: #21689b;
		font-family: 'Source Code Pro', 'monospace';

		text-wrap: nowrap;
		overflow: scroll;
		scrollbar-width: thin;

		text-align: right;
	}

	a.file-entry > .left {
		text-wrap: nowrap;
		overflow: scroll;
		scrollbar-width: thin;
		mask-image: linear-gradient(to right, #fff 80%, transparent);
	}

	a.file-entry:hover {
		text-decoration: underline;
	}

	details > form {
		display: flex;
		align-items: flex-end;
		padding: 1rem;
		background-color: #f8f8f8;
		margin-top: 0.5rem;
	}

	form.mimeupdate {
		flex-direction: column;
		gap: 0.5rem;
	}

	form.mimeupdate > * {
		width: 100%;
	}

	form.mimeupdate > div {
		display: flex;
		align-items: center;
		gap: 1rem;
		width: 100%;
	}

	input[type='text'] {
		flex-grow: 1;
		font-family: inherit;
		font-size: inherit;
		padding: 10px;
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

	div.panel {
		display: flex;
		align-items: flex-start;
		gap: 0.5rem;

		padding: 1rem;

		background-color: #f8f8f8;
	}

	div.sort-buttons {
		display: flex;
		flex-direction: column;
		align-items: end;
		gap: 0.5rem;
	}

	@media not (max-width: 860px) {
		div.panel {
			margin-top: 0.5rem;
		}

		a.file-entry {
			grid-template-columns: 1fr auto;
		}
	}
</style>
