import { error } from '@sveltejs/kit';
import type { PageLoad } from './$types';

export const load: PageLoad = async ({ fetch, url }) => {
	try {
		const md = await fetch('/api/metadata');
		if (!md.ok) {
			throw Error(`Response status: ${md.status}`)
		}

		return {
			backendMetadata: await md.json()
		};
	} catch (e) {
		error(500, `Couldn't fetch metadata from backend API: ${e.message}`)
	}
};
