import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.util.Scanner;

public class ToMetaPostProc {
	private final static String ESC_SYM = "_e";
	private final static String LIFT_SYM = "_l";

	public static void main(String[] args) throws Exception {
                String ppxOutFile = args[0];
		
		Scanner sc = new Scanner(new BufferedReader(new FileReader(new File(ppxOutFile))));
		PrintWriter pr = new PrintWriter(new BufferedWriter(new OutputStreamWriter(System.out)));

		while (sc.hasNextLine()) {
			String line = sc.nextLine();
			line = line.replaceAll(ESC_SYM, ".~");
			if (line.contains(LIFT_SYM)) line = insertBrackets(line);
			pr.println(line);
		}

		sc.close();
		pr.close();
	}

	static String insertBrackets(String line) {
		line = line + " ";
		while (line.contains(LIFT_SYM)) {
			int idx = line.indexOf(LIFT_SYM);
			line = line.replaceFirst(LIFT_SYM, ".<");
			idx += 2;

			while (line.charAt(idx) == ' ' ) idx++;

			if (line.charAt(idx) == '(') {
				int count = 0;
				while (true) {
					if (line.charAt(idx) == '(') count++;
					else if (line.charAt(idx) == ')') count--;
				
					if (count == 0) break;

					idx++;
				}
				idx++;
			} else {
				while (line.charAt(idx) != ' ' && line.charAt(idx) != ')') idx++;
				idx--;
			}
			line = line.substring(0, idx + 1) + ">." + line.substring(idx + 1);
		}
		return line;
	}
}

