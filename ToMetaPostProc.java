import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.util.LinkedList;
import java.util.Queue;
import java.util.Scanner;

public class ToMetaPostProc {
	public static void main(String[] args) throws Exception {
                String ppxOutFile = args[0];
		
		Scanner sc = new Scanner(new BufferedReader(new FileReader(new File(ppxOutFile))));
		PrintWriter pr = new PrintWriter(new BufferedWriter(new OutputStreamWriter(System.out)));

		while (sc.hasNextLine()) {
			String line = sc.nextLine();
			line = line.replaceAll("lift", ".<");
			line = line.replaceAll("esc", ".~");
			pr.println(line);
		}

		sc.close();
		pr.close();
	}
}

