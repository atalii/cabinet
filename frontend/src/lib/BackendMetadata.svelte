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
		return `${Math.round(bytes)}${unit}`;
	};
</script>

<div class="infobox">
	<div class="attr">
		<span class="key">Total Stored</span>
		<span class="val">{formatByteValue(data.in_use)}</span>
	</div>
</div>

<style>
	div.infobox {
		width: calc(100% - 40px);
		background-color: #ffffff;
		display: flex;
		border: 1px solid #f6ae2d;
		overflow: scroll;
		padding: 20px;
	}

	div.attr {
		width: 100%;
		display: flex;
		justify-content: space-between;
	}
</style>
