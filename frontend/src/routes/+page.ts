import type { PageLoad } from './$types';

const fromStatus: String = (val) => {
	if (!val) {
		return null;
	}

	switch (val) {
		case 'UploadOk':
			return 'Successfully uploaded files.';
		case 'UploadEmpty':
			return 'Cannot upload an empty file.';
		case 'NoFiles':
			return 'No files were uploaded.';
		default:
			return `Unrecognized status ${val}`;
	}
};

export const load: PageLoad = async ({ fetch, url }) => {
	try {
		const status = url.searchParams.get('status');
		const index = await fetch('/api/index');
		if (!index.ok) {
			throw Error(`Response status: ${md.status}`)
		}

		return {
			success: status === 'UploadOk',
			message: fromStatus(status),
			index: await index.json()
		};
	} catch (e) {
		error(500, `Couldn't fetch index from backend API: ${e.message}`)
	}
};
