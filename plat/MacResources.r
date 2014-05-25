#include "Balloons.r"

#define kIconHelpString 556
#define kJZINTVSignature 'INTv'

resource 'WIND' (4000) {
	{68, 26, 268, 346},
	noGrowDocProc,
	invisible,
	noGoAway,
	0x0,
	"jzintv",
	noAutoCenter
};

resource 'clut' (130) {
	{	/* array ColorSpec: 17 elements */
		/* [1] */
		0, 0, 0,
		/* [2] */
		0, 0, 65535,
		/* [3] */
		65535, 0, 0,
		/* [4] */
		57670, 41287, 26869,
		/* [5] */
		0, 29555, 0,
		/* [6] */
		0, 51143, 0,
		/* [7] */
		65535, 65535, 0,
		/* [8] */
		65535, 65535, 65535,
		/* [9] */
		49087, 49087, 49087,
		/* [10] */
		0, 34181, 34181,
		/* [11] */
		65535, 37265, 0,
		/* [12] */
		41287, 25558, 0,
		/* [13] */
		65535, 28784, 65535,
		/* [14] */
		37265, 65535, 65535,
		/* [15] */
		31354, 65535, 0,
		/* [16] */
		43947, 0, 65535,
		/* [17] */
		0, 0, 0
	}
};

resource 'MENU' (128, "Apple Menu") {
	128,
	textMenuProc,
	0x7FFFFFFC,
	enabled,
	apple,
	{	/* array: 2 elements */
		/* [1] */
		"About Macjzintv", noIcon, noKey, noMark, plain,
		/* [2] */
		"-", noIcon, noKey, noMark, plain
	}
};

resource 'MENU' (129) {
	129,
	textMenuProc,
	0x7FFFFFC0,
	enabled,
	"File",
	{	/* array: 7 elements */
		/* [1] */
		"New", noIcon, "N", noMark, plain,
		/* [2] */
		"Open…", noIcon, "O", noMark, plain,
		/* [3] */
		"Save…", noIcon, "S", noMark, plain,
		/* [4] */
		"-", noIcon, noKey, noMark, plain,
		/* [5] */
		"Close", noIcon, "W", noMark, plain,
		/* [6] */
		"-", noIcon, noKey, noMark, plain,
		/* [7] */
		"Quit", noIcon, "Q", noMark, plain
	}
};

resource 'MENU' (130) {
	130,
	textMenuProc,
	0x7FFFFF00,
	enabled,
	"Edit",
	{	/* array: 8 elements */
		/* [1] */
		"Undo", noIcon, noKey, noMark, plain,
		/* [2] */
		"-", noIcon, noKey, noMark, plain,
		/* [3] */
		"Cut", noIcon, "X", noMark, plain,
		/* [4] */
		"Copy", noIcon, "C", noMark, plain,
		/* [5] */
		"Paste", noIcon, "V", noMark, plain,
		/* [6] */
		"Clear", noIcon, noKey, noMark, plain,
		/* [7] */
		"-", noIcon, noKey, noMark, plain,
		/* [8] */
		"Preferences…", noIcon, noKey, noMark, plain
	}
};

resource 'MENU' (131) {
	131,
	textMenuProc,
	0x7FFFFFC0,
	enabled,
	"Game",
	{	/* array: 6 elements */
		/* [1] */
		"Standard Window", noIcon, noKey, noMark, plain,
		/* [2] */
		"Double Window", noIcon, noKey, noMark, plain,
		/* [3] */
		"-", noIcon, noKey, noMark, plain,
		/* [4] */
		"Audio Off", noIcon, noKey, noMark, plain,
		/* [5] */
		"Audio 22050 MHz", noIcon, noKey, noMark, plain,
		/* [6] */
		"Audio 44100 MHz", noIcon, noKey, noMark, plain
	}
};

resource 'MBAR' (128) {
	{	/* array MenuArray: 4 elements */
		/* [1] */
		128,
		/* [2] */
		129,
		/* [3] */
		130,
		/* [4] */
		131
	}
};

resource 'hfdr' (-5696, purgeable) { /* Help for jzintv icon */
	HelpMgrVersion, hmDefaultOptions, 0, 0,  /* Header information */
	{ HMSTRResItem {kIconHelpString}}
};

resource 'STR ' (kIconHelpString, purgeable ) {  /* Help message for app icon */
	"Use jzintv to play your excellent Intellivision game ROMS."
};

resource 'kind' (128, purgeable ) {
	kJZINTVSignature,
	0,  /* region code */
	{
		'BINA', "Intellivision ROM",
		'pBIN', "Intellivision ROM",
		'SVGM', "Intellivision ROM",
	},
};

resource 'open' (128, purgeable ) {
	kJZINTVSignature,
	{
		'BINA', 'pBIN', 'SVGM',
	}
};
