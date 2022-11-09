package b_bayesian_network

import guru.nidi.graphviz.engine.{Format, Graphviz}
import guru.nidi.graphviz.parse.Parser
import org.jgrapht.Graph
import org.jgrapht.graph.DefaultEdge
import org.jgrapht.nio.{Attribute, DefaultAttribute}
import org.jgrapht.nio.dot.DOTExporter

import java.io.{File, StringWriter}

object GraphTools {

  var default_file = new File("graph.png")

  def renderToFile(g: Graph[Variable, DefaultEdge], file: File = default_file) = {
    val exporter = new DOTExporter[Variable, DefaultEdge]((v) => v.name)
    exporter.setVertexAttributeProvider(v =>
      new java.util.LinkedHashMap[String, Attribute] {
        put("label", DefaultAttribute.createAttribute(v.name))
      }
    )

    val writer = new StringWriter
    exporter.exportGraph(g, writer)
    System.out.println(writer.toString)

    val gwg = new Parser().read(writer.toString)
    Graphviz.fromGraph(gwg).width(700).render(Format.PNG).toFile(file);
  }
}
