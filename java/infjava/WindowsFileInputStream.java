
import java.io.*;
import org.antlr.v4.runtime.*;

public class WindowsFileInputStream extends ANTLRFileStream
{
    // Use the following elisp expression to change eol characters
    // when testing: (set-buffer-file-coding-system 'dos)
    
    public WindowsFileInputStream(String filename) throws IOException
    {
	super(filename);
	String temp = new String(data).replaceAll("\\r", "");
	data = temp.toCharArray();
	n = temp.length();
    }

}
