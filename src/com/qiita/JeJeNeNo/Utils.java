package com.qiita.JeJeNeNo;

// Acknowledge: https://qiita.com/JeJeNeNo/items/7c7e99d90798b9261bb8
// tweaked for Munsellpicker by Hugo I.

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.stream.Stream;

public class Utils {
	/**
	 * list up files with specified file extension (extension)
	 * under the specified directory (rootDir).
	 * @param rootDir   : root directory
	 * @param extension : string needs to be contained as file extension
	 * @param recursive : scan roodDir recursively if true
	 * @return          : list of files
	 */
	public static ArrayList<File> listUpFiles(Path rootDir, String extension, boolean recursive){
		String extensionPattern = "." + extension.toLowerCase();
		final ArrayList<File> fileList = new ArrayList<File>();

		try (final Stream<Path> pathStream = (recursive ? Files.walk(rootDir) : Files.list(rootDir))) {
			pathStream
			.map(Path::toFile)
			.filter(file -> !file.isDirectory())
			.filter(file -> file.getName().toLowerCase().endsWith(extensionPattern))
			.forEach(fileList::add);
		} catch (final IOException e) {
			e.printStackTrace();
		}
		return (fileList);
	}
}
