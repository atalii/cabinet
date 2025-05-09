import type { PageLoad } from './$types';

const fromStatus: String = (val) => {
	if (!val) {
		return null;
	}

	switch (val) {
		case "UploadOk":
			return "Successfully uploaded files.";
		case "UploadEmpty":
			return "Cannot upload an empty file.";
		case "NoFiles":
			return "No files were uploaded.";
		default:
			return `Unrecognized status ${val}`;
	}
}

export const load: PageLoad = async ({ fetch, url }) => {
	const status = url.searchParams.get("status");
	const index = await fetch('/api/index');

	return {
		success: status === "UploadOk",
		message: fromStatus(status),
		index: await index.json(),
	};
}
