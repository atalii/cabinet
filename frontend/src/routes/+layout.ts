import type { PageLoad } from './$types';

export const load: PageLoad = async ({ fetch, url }) => {
	const md = await fetch('/api/metadata');

	return {
		backendMetadata: await md.json()
	};
};
