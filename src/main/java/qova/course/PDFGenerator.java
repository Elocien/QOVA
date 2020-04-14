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


import java.io.File;
import java.io.IOException;
import java.time.format.TextStyle;
import java.util.HashMap;
import java.util.Map;
 
/**
 * Simple Hello World example.
 */
public class PDFGenerator {
    
    public static final String DEST = "src/main/resources/test.pdf";
    
    public static void main(String args[]) throws IOException {
        File file = new File(DEST);
        file.getParentFile().mkdirs();
        new PDFGenerator().createPdf(DEST);
    }
    
    public void createPdf(String dest) throws IOException {

        //Test iteration through arraylist
        Map<String, String> a = new HashMap<>();
        a.put("1", "Some response");
        a.put("2", "this is a text response");
        
        

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
        for (String i : a.values()) {
            document.add(new Paragraph(i));
        }		






        // Add the list
        document.add(list);


 
        //Close document
        document.close();
    }
}