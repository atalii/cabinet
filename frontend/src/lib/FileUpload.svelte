<script lang="ts">
	import Dropzone from 'svelte-file-dropzone';

	interface Files {
		accepted: any[];
		rejected: any[];
	}
	let files: Files = $state({
		accepted: [],
		rejected: []
	});

	function handleFilesSelect(e) {
		const { acceptedFiles, fileRejections } = e.detail;
		files.accepted = acceptedFiles;
		files.rejected = fileRejections;
	}
</script>

<form action="/api/files/upload" method="post" enctype="multipart/form-data">
	<div class="file-menu">
		<Dropzone containerClasses="file-input" on:drop={handleFilesSelect} name="files">
			<span class="form-copy">Select files.</span>
		</Dropzone>

		<input type="submit" class="submit" value="Upload files." />
	</div>

	<ul class="files">
		{#each files.accepted as file}
			<li>{file.path}</li>
		{/each}
	</ul>
	<ul class="files">
		{#each files.rejected as file}
			<li>{file.path}</li>
		{/each}
	</ul>
</form>

<style>
	.file-menu {
		display: flex;
		flex-direction: row;
		column-gap: 0.35rem;
	}

	.submit {
		border: 1px solid #5b5bbb;
		border-radius: 0.25rem;
		background-color: #eaffe4;
		padding-left: 1rem;
		padding-right: 1rem;

		font-size: inherit;
	}

	.submit:hover {
		cursor: pointer;
	}

	:global {
		.file-input {
			border: 1px solid #5b5bbb !important;
			border-radius: 0.25rem !important;
			background-color: #f2f2f2 !important;
			align-items: start !important;
			padding: 1rem !important;
		}

		.file-input:hover {
			cursor: pointer;
		}

		/* Using :has makes sure that we don't erroneously style empty
		 * .files. */
		.files:has(li) {
			max-width: 100%;
			padding: 1rem;
			margin-top: 1rem;
			background-color: #f9f9f9;
			border: 1px solid #5b5b5b;
			border-radius: 0.25rem;

			display: flex;
			row-gap: 0.35rem;
			flex-direction: column;
		}

		.files > li {
			list-style: none;
		}

		.files > li:hover {
			color: #22391b;
		}
	}

	.form-copy {
		color: black;
	}
</style>
