import os
import shutil
import urllib

def getmp3():
	for root, dirs, files in os.walk('.'):
		for filename in files:
			if os.path.splitext(filename)[1] == ".mp3":
				yield os.path.join(root, filename)

def main () :
	# Moves a custom mp3 into the source directory to play
	for mp3file in getmp3():
		shutil.copyfile(mp3file, "soundfile.mp3")
		shutil.move(os.path.join(".", "soundfile.mp3"), os.path.join("src/", "soundfile.mp3"))
		break


if __name__ == "__main__":
    main()
