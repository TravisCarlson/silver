package common;

import java.io.*;
import java.nio.channels.FileChannel;
import java.nio.file.DirectoryStream;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.LinkOption;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.*;

import common.exceptions.*;

import core.NLocation;
import core.NParseError;
import core.NParseResult;
import core.Ploc;
import core.PparseFailed;
import core.PsyntaxError;
import core.PunknownParseError;
import edu.umn.cs.melt.copper.runtime.engines.CopperParser;
import edu.umn.cs.melt.copper.runtime.logging.CopperParserException;
import edu.umn.cs.melt.copper.runtime.logging.CopperSyntaxError;


/**
 * Many places in Silver's translation are bits of code that need factoring out, somehow.
 * 
 * <p>The most common justification is the need to do several statements, while being in the middle
 * of an expression.
 * 
 * @author tedinski, bodin
 */
public final class Util {
	/**
	 * Ensures that a (potential) closure is evaluated.
	 *
	 * Use by writing  (v = Util.demand(v)), never just invoke it!
	 *
	 * @param c  Either a value, or a Closure
	 * @return The value, either directly, or evaluating the Closure
	 */
	public static Object demand(Object c) {
		if(c instanceof Thunk)
			return ((Thunk<?>)c).eval();
		return c;
	}

	/**
	 * Turns a list of names and values into a map.
	 * 
	 * <p>Used by the 'decorate ... with { THIS PART }' syntax.
	 */
	public static Lazy[] populateInh(final int size, final int[] idx, final Lazy[] val) {
		final Lazy[] result = new Lazy[size];
		for(int i = 0; i < idx.length; i++) {
			result[idx[i]] = val[i];
		}
		return result;
	}

	/**
	 * Exit, of course!
	 * 
	 * <p>This is here because it has to return Object to be used in expressions.
	 * 
	 * @param status the exit status code
	 * @return Does not return.
	 */
	public static Object exit(int status) {
		throw new SilverExit(status);
	}
	
	/**
	 * Used by the 'error("wat")' syntax in Silver.
	 * 
	 * @param o the "wat"
	 * @return Does not return.
	 */
	public static Object error(Object o) {
		System.err.print(o);
		throw new SilverError(o.toString());
	}
	
	public static core.NMaybe safetoInt(String s) {
		try {
			return new core.Pjust( Integer.valueOf(s) );
		} catch(NumberFormatException e) {
			return new core.Pnothing();
		}
	}

	public static boolean isAlpha(String sb) {
		boolean result = true;
		for (int i = 0; result && i < sb.length(); i++) {
			result = Character.isLetter(sb.charAt(i));
		}
		return result;
	}

	public static boolean isDigit(String sb) {
		boolean result = true;
		for (int i = 0; result && i < sb.length(); i++) {
			result = Character.isDigit(sb.charAt(i));
		}
		return result;
	}

	public static boolean isSpace(String sb) {
		boolean result = true;
		for (int i = 0; result && i < sb.length(); i++) {
			result = Character.isWhitespace(sb.charAt(i));
		}
		return result;
	}

	public static boolean isUpper(String sb) {
		boolean result = true;
		for (int i = 0; result && i < sb.length(); i++) {
			result = Character.isUpperCase(sb.charAt(i));
		}
		return result;
	}

	public static boolean isLower(String sb) {
		boolean result = true;
		for (int i = 0; result && i < sb.length(); i++) {
			result = Character.isLowerCase(sb.charAt(i));
		}
		return result;
	}

	public static int fileTime(String sb) {
		return (int) ((new File(sb).lastModified()) / 1000);
	}
	
	public static Object touchFile(String sb) {
		return setFileTime(sb, currentTime());
	}
	
	public static Object setFileTime(String sb, int time) {
		new File(sb).setLastModified(((long)time) * 1000);
		return null;
	}
	
	public static int currentTime() {
		return (int)(System.currentTimeMillis() / 1000);
	}

	public static boolean isFile(String sb) {
		return new File(sb).isFile();
	}

	public static boolean isDirectory(String sb) {
		return new File(sb).isDirectory();
	}
	
	public static boolean mkdir(String sb) {
		return new File(sb).mkdirs();
	}

	public static boolean deleteFile(String sb) {
		return new File(sb).delete();
	}
	
	public static Object deleteTree(String path) {
		// We should consider using walkFileTree, in the future
		deleteTreeRecursive(Paths.get(path));
		return null;
	}
	private static void deleteTreeRecursive(Path f) {
		if(!Files.exists(f, LinkOption.NOFOLLOW_LINKS)) {
			// If the file doesn't exist, ignore it silently. Disk changed underneath us or something?
			// NOFOLLOW because we care if the symlink exists, not what the symlink points to.
			return;
		}
		// Juuuust in case, we should never be trying to delete "/" or "/home" or "c:/users" etc.
		if(f.toAbsolutePath().normalize().toString().length() < 9)
			throw new RuntimeException("Canary against deleting things we shouldn't. Tried to delete path: " + f);
		try {
			// Need to handle: symlinks, directories, and normal files.
			if(Files.isSymbolicLink(f)) {
				// Deletes symlink, without traversing into it.
				Files.delete(f);
			} else if(Files.isDirectory(f)) {
				// Recursively delete files, then the directory itself.
				try (DirectoryStream<Path> stream = Files.newDirectoryStream(f)) {
					for (Path child : stream) {
						deleteTreeRecursive(child);
					}
				}
			} else {
				Files.delete(f);
			}
		} catch (IOException e) {
			// If we encounter an IO error anywhere, we immediately stop and raise an exception
			// Safety valve if we try to delete something we shouldn't.
			throw new RuntimeException(e);
		}
	}
	
	private static BufferedReader our_stdin = null;
	public static StringCatter getStr() {
		try {
			if(our_stdin == null) {
				// We should persist this, since it's buffered, we might buffer bytes for the NEXT line
				our_stdin = new BufferedReader(new InputStreamReader(System.in));
			}
			return new StringCatter(our_stdin.readLine()) ;
		} catch (IOException e) {
			throw new RuntimeException(e);
		}
	}

	/**
	 * Copy a file from 'from' to 'to'.
	 * 
	 * @param from A path to the file to copy
	 * @param to A path to where the file should be copied. May be a directory.
	 * @return null IO token.
	 */
	public static Object copyFile(String from, String to) {
		Path src = Paths.get(from);
		Path dst = Paths.get(to);
		try {
			// copy x.java into src. create the file src/x.java. Works even if dst is symlink.
			if(Files.isDirectory(dst)) {
				dst = dst.resolve(dst.getFileName());
			}
			Files.copy(src, dst);
		} catch (IOException io) {
			// Unfortunately, we still don't have a better way.
			throw new RuntimeException(io);
		}
		return null;
	}
	
	/**
	 * Slurps the contents of a file into a string.  May cause IO exceptions.
	 * 
	 * @param sb  The filename
	 * @return  The file contents.
	 */
	public static StringCatter readFile(String filename) {
		try {
			byte[] b = Files.readAllBytes(Paths.get(filename));
			return new StringCatter(new String(b));
		} catch (Exception e) {
			throw new RuntimeException(e);
		}
	}

	public static StringCatter cwd() {
		// This is a typical approach because JVMs don't change the working directory.
		// However, this is an alternative if there's ever a reason to suspect that this no longer works
		// Paths.get(".").toAbsolutePath().normalize().toString()
		return new StringCatter(System.getProperty("user.dir"));
	}
	
	/**
	 * We have a (public) copy of the environment because RunSilver may need to modify it.
	 * 
	 * Sadly, there does not appear to be any way to do with without making our own copy
	 * of the entire environment.
	 */
	public static Map<String, String> environment = new TreeMap<String, String>(System.getenv());

	public static StringCatter env(String sb) {
		String result = environment.get(sb);
		if (result == null)
			return new StringCatter(""); // Is this the right reply?
		else
			return new StringCatter(result);
	}

	/**
	 * Invokes an external command, channeling all stdin/out/err to the console normally.
	 * 
	 * N.B. uses 'bash' to invoke the command. There are two major reasons:
	 * (1) allows redirects and such, which is useful
	 * (2) because this command takes just a single string, we must somehow deal with spaces.
	 * e.g. 'touch "abc 123"' bash take care of interpreting the quotes for us.
	 * 
	 * Unfortunately platform dependency though.
	 * 
	 * @param sb A string for back to interpret and execute.
	 * @return The exit status of the process.
	 */
	public static int system(String sb) {
		try {
			ProcessBuilder pb = new ProcessBuilder("bash", "-c", sb);
			pb.inheritIO();
			Process p = pb.start();
			p.waitFor();
			return p.exitValue();
		} catch (Exception e) {
			throw new RuntimeException(e);
		}
	}

	/**
	 * Write to a file, truncating anything there already. Used by 'writeFile' in silver.
	 * 
	 * <p>Avoids demanding a StringCatter.
	 * 
	 * @param file The filename
	 * @param content Either a String or {@link StringCatter} object.
	 * @return null, the IO object.
	 */
	public static Object writeFile(String file, Object content) {
		try {
			Writer fout = new FileWriter(file); // already buffered
			if(content instanceof StringCatter)
				((StringCatter)content).write(fout);
			else
				fout.write(content.toString());
			fout.flush();
			fout.close();
			return null;
		} catch (Exception e) {
			throw new RuntimeException(e);
		}
	}

	/**
	 * Write to a file, appending onto the end of anything there already. Used by 'appendFile' in silver.
	 * 
	 * <p>Avoids demanding a StringCatter.
	 * 
	 * @param file The filename
	 * @param content Either a String or {@link StringCatter} object.
	 * @return null, the IO object.
	 */
	public static Object appendFile(String file, Object content) { // TODO: merge with above!
		try {
			Writer fout = new FileWriter(file, true); // already buffered
			if(content instanceof StringCatter)
				((StringCatter)content).write(fout);
			else
				fout.write(content.toString());
			fout.flush();
			fout.close();
			return null;
		} catch (Exception e) {
			throw new RuntimeException(e);
		}
	}

	public static Object print(String sb) {
		// TODO: should we avoid demanding stringcatter objects?
		System.out.print(sb);
		return null;
	}

	/**
	 * Lists the contents of a directory.
	 * 
	 * @param sb The directory to list the contents of.
	 * @return A list of Strings
	 */
	public static ConsCell listContents(String sb) {
		try {
			File f = new File(sb);
			String[] files = f.list();

			ConsCell result = ConsCell.nil;
			
			if(files == null)
				return result;
			
			for (String file : files) {
				result = new ConsCell(new StringCatter(file), result);
			}
			
			return result;
			
		} catch (Exception e) {
			throw new RuntimeException(e);
		}
	}

	/**
	 * This exists only because some things like {@link Util#writeFile} don't take an IO object, so
	 * we use this to demand the io object, then call writeFile.
	 * 
	 * <p>In other cases, we're actually returning something, like system, and the call to the IOInteger
	 * nonterminal constructor takes care of demanding the old IO object.
	 * 
	 * @param i First thing to do
	 * @param o Second thing to do
	 * @return null, the IO object.
	 */
	public static Object io(Object i, Object o) {
		return null;
	}

	private static int i = 0;
	public static int genInt() {
		return i++;
	}
	
	public static void printStackCauses(Throwable e) {
		System.err.println("\nAn error occured.  Silver stack trace follows. (To see full traces including java elements, SILVERTRACE=1)\n");
		
		if(! "1".equals(System.getenv("SILVERTRACE"))) {
			Throwable t = e;
			while(t != null) {
				StackTraceElement st[] = t.getStackTrace();
				
				String msg = t.getLocalizedMessage();
				if(msg == null) // Some exceptions have no message... apparently.
					msg = t.toString();
				
				if(st.length == 0) {
					// Some exceptions don't seem to occur anywhere... somehow.
					System.err.println("(??): " + msg);
				} else if(st[0].getClassName().startsWith("common.")) {
					// This will give error messages like (DN.146) corresponding to DecoratedNode.java:146
					System.err.println("(" + st[0].getFileName().replaceAll("[a-z]", "") + st[0].getLineNumber() + "): " + msg);
				} else {
					// Be more explicit about where when it's not one of ours
					System.err.println("(" + st[0].getClassName() + " in " + st[0].getFileName() + ":" + st[0].getLineNumber() + "): " + msg);
					if(t instanceof NullPointerException && st.length > 1) {
						System.err.println("\t1 up: " + st[1].getClassName() + " in " + st[1].getFileName() + ":" + st[1].getLineNumber());
						if(st.length > 2)
							System.err.println("\t2 up: " + st[2].getClassName() + " in " + st[2].getFileName() + ":" + st[2].getLineNumber());
					}
				}
				
				String lastCause = t.getLocalizedMessage();
				int repeats = 0;
				t = t.getCause();
				while(t != null && lastCause.equals(t.getLocalizedMessage())) {
					repeats++;
					t = t.getCause();
				}
				if(repeats > 0) {
					System.err.println("\t(last line repeats " + repeats + " more times)");
				}
			}
			
			System.exit(-2);		
		} else {
			// Displaying it by rethrowing it.
			throw new RuntimeException(e);
		}
	}
	
	// These are written un-ideally so that they're all confined in one place.
	public static StringCatter hackyhackyUnparse(Object o) {
		StringBuilder sb = new StringBuilder();
		
		hackyhackyUnparseObject(o, sb);
		
		return new StringCatter(sb.toString());
	}
	
	private static void hackyhackyUnparseObject(Object o, StringBuilder sb) {
		if(o instanceof Node) {
			hackyhackyUnparseNode((Node)o, sb);
		} else if(o instanceof DecoratedNode) {
			// For the time being, just undecorate it
			hackyhackyUnparseNode(((DecoratedNode)o).undecorate(), sb);
		} else if(o instanceof TerminalRecord) {
			TerminalRecord t = (TerminalRecord) o;
			sb.append("'" + t.lexeme + "'");
		} else if(o instanceof StringCatter) {
			sb.append("\"" + o.toString() + "\"");
		} else if(o instanceof Integer ||
			  o instanceof Float ||
 			  o instanceof Boolean) {
			sb.append(o.toString());
		} else if(o instanceof ConsCell) {
			hackyhackyUnparseList((ConsCell)o, sb);
		} else {
			sb.append("<OBJ>");
		}
	}
	private static void hackyhackyUnparseNode(Node n, StringBuilder sb) {
		sb.append(n.getName() + "(");
		for(int i = 0; i < n.getNumberOfChildren(); i++) {
			if(i != 0) {
				sb.append(", ");
			}
			hackyhackyUnparseObject(n.getChild(i), sb);
			//System.out.println(sb.toString());
		}
		sb.append(")");
	}
	private static void hackyhackyUnparseList(ConsCell c, StringBuilder sb) {
		sb.append("[");
		ConsCell i = c;
		while(!i.nil()) {
			if(i != c) {
				sb.append(", ");
			}
			hackyhackyUnparseObject(i.head(), sb);
			i = i.tail();
		}
		sb.append("]");
	}
	
	/**
	 * Calls a Copper parser, and returns a ParseResult<ROOT> object.
	 * 
	 * @param parser The Copper parser to call
	 * @param string The string to parse.
	 * @param file The filename to report to the parser (filling in location information)
	 * @return A silver ParseResult<ROOT> node.
	 */
	public static <ROOT> NParseResult callCopperParser(CopperParser<ROOT, CopperParserException> parser, Object string, Object file) {
		String javaString = ((StringCatter)demand(string)).toString();
		String javaFile = ((StringCatter)demand(file)).toString();
		try {
			return new core.PparseSucceeded(parser.parse(new StringReader(javaString), javaFile));
		} catch(CopperSyntaxError e) {
			// To create a space, we increment the ending columns and indexes by 1.
			NLocation loc = new Ploc(new StringCatter(e.getVirtualFileName()), e.getVirtualLine(), e.getVirtualColumn(), e.getVirtualColumn(), e.getVirtualColumn() + 1, (int)(e.getRealCharIndex()), (int)(e.getRealCharIndex()) + 1);
			NParseError err = new PsyntaxError(
					new common.StringCatter(e.getMessage()),
					loc,
					convertStrings(e.getExpectedTerminalsDisplay().iterator()),
					convertStrings(e.getMatchedTerminalsDisplay().iterator()));
			return new PparseFailed(err);
		} catch(CopperParserException e) {
			// Currently this is dead code, but perhaps in the future we'll see IOException wrapped in here.
			NParseError err = new PunknownParseError(new StringCatter(e.getMessage()), file);
			return new PparseFailed(err);
		} catch(Throwable t) {
			throw new TraceException("An error occured while parsing", t);
		}
	}
	
	/**
	 * Like javainterop.ConsCellCollection.fromIterator, but also converts String to StringCatter.
	 */
	private static ConsCell convertStrings(Iterator<String> i) {
		if(!i.hasNext())
			return ConsCell.nil;
		return new ConsCell(new StringCatter(i.next()), convertStrings(i));
	}

}
