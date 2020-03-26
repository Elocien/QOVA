package qova.course;

import com.itextpdf.io.font.constants.StandardFonts;
import com.itextpdf.kernel.font.PdfFont;
import com.itextpdf.kernel.font.PdfFontFactory;
import com.itextpdf.kernel.pdf.PdfDocument;
import com.itextpdf.kernel.pdf.PdfWriter;
import com.itextpdf.layout.Document;
import com.itextpdf.layout.element.List;
import com.itextpdf.layout.element.ListItem;
import com.itextpdf.layout.element.Paragraph;

import org.javatuples.Pair;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
 
/**
 * Simple Hello World example.
 */
public class PDFGenerator {
    
    public static final String DEST = "src/main/resources/helloworld.pdf";
    
    public static void main(String args[]) throws IOException {
        File file = new File(DEST);
        file.getParentFile().mkdirs();
        new PDFGenerator().createPdf(DEST);
    }
    
    public void createPdf(String dest) throws IOException {

        //Test iteration through arraylist
        Pair<String, Integer> tuple1 = new Pair<String, Integer>("text response", 1);
        Pair<String, Integer> tuple2 = new Pair<String, Integer>("more text", 2);
        ArrayList<Pair> a = new ArrayList<Pair>();
        a.add(tuple1);
        a.add(tuple2);

        //Initialize PDF writer
        PdfWriter writer = new PdfWriter(dest);
 
        //Initialize PDF document
        PdfDocument pdf = new PdfDocument(writer);
        
        // Initialize document
        Document document = new Document(pdf);
 


        // Create a PdfFont
        PdfFont font = PdfFontFactory.createFont(StandardFonts.TIMES_ROMAN);
        // Add a Paragraph
        document.add(new Paragraph("iText is:").setFont(font));
        // Create a List
        List list = new List()
            .setSymbolIndent(12)
            .setListSymbol("\u2022")
            .setFont(font);




        //TODO

        for (int counter = 0; counter < a.size(); counter++) { 		      
            // list.add(new ListItem((a.get(counter)).getValue0());
        }   		






        // Add the list
        document.add(list);




        //Add paragraph to the document
        // document.add(new Paragraph("Hello World!"));
 
        //Close document
        document.close();
    }
}