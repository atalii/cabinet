<script lang="ts">
	const props = $props();
	let data = props.data;

	const formatByteValue = (bytes: number) => {
		const prefixes = ['B', 'KB', 'MB', 'GB', 'TB'];
		let prefixIdx = 0;
		while (bytes >= 1024 && prefixIdx < prefixes.length) {
			prefixIdx++;
			bytes /= 1024;
		}

		const unit = prefixes[prefixIdx];
		return `${bytes.toPrecision(4)}${unit}`;
	};
</script>

<div class="infobox">
	<div class="attr">
		<span class="key">Total Stored</span>
		<span class="val">{formatByteValue(data.in_use)}</span>

		<span class="key">Stored at Last GC</span>
		<span class="val">{formatByteValue(data.in_use_at_last_gc)}</span>

		<span class="key">GC Interval</span>
		<span class="val">{formatByteValue(data.gc_interval)}</span>

		<span class="key">GC Proportion</span>
		<span class="val">{data.gc_prop}&percnt;</span>
	</div>
	<form action="/api/gc/trigger" method="post">
		<input type="submit" value="Trigger GC." />
	</form>
</div>

<style>
	div.infobox > * {
		background-color: #ffffff;
		border: 1px solid #f6ae2d;
		padding: 20px;
		width: 100%;
	}

	div.infobox > div.attr {
		width: calc(100% - 40px);
	}

	div.infobox > form {
		padding: 0;
		margin: 0;
	}

	div.infobox {
		width: 100%;
		display: flex;
		flex-direction: column;
		row-gap: 20px;
	}

	span.val {
		text-align: right;
	}

	input[type='submit'] {
		padding: 20px;
		width: 100%;
		height: 100%;
		font-family: inherit;
		font-size: inherit;
		text-align: left;
		border: none;
		background: none;
	}
	input[type='submit']:hover {
		cursor: pointer;
	}

	div.attr {
		display: grid;
		grid-template-columns: 1fr auto;
		justify-content: space-between;
	}
</style>
