# elm-rhythm
A rhythm-game generator in Elm using real-time audio

How to run:

If using Mac OS:

	Run the HTML directly:
		1) Change into the src/ directory.
		2) Open the out.html file in Firefox.
		This will play the default soundfile supplied in our repository.

	Add your own mp3 sound file:
		1) Paste an mp3 file into the repository's root (the folder with update.txt)
		2) Run 'python importSound.py' in the terminal.
		3) Open the out.html file as above.


If using Linux (Ubuntu):

	By script:
		1) Stay in the root directory (same folder as update.txt)
		2) Enter '.\run.sh' in the terminal.
		An instance of Firefox will be opened automatically with our html page.

	Adding your own sound mp3 file:
		1) Paste an mp3 file in the directory's root (same folder as update.txt).
		2) Run '.\run.sh' in the terminal.
		The default sound file will be overwritten and replaced with your mp3.
		Then, an instance of Firefox will be opened automatically.


If using Windows or as a default:

	Run the HTML directly:
		1) Change into the src\ directory.
		2) Open the out.html file in Firefox.
		This will play the default soundfile supplied in our repository.

	Adding your own sound mp3 file:
		1) Delete "soundfile.mp3" from the directory's src\ folder.
		2) Paste an mp3 file of your choice in src\ instead.
		3) Rename said mp3 to "soundfile.mp3".
		4) Open out.html in Firefox as above.
