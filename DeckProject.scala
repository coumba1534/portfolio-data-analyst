import org.apache.spark.sql.{Dataset, SparkSession}
import org.apache.spark.sql.functions._

case class Card(color: String, suit: String, value: String)
object DeckProject {
  def main(args: Array[String]): Unit = {
    val spark = SparkSession.builder()
      .appName("DeckProject")
      .master("local[*]")
      .getOrCreate()

    import spark.implicits._

    //Chargement et Modélisation
    // 1. Lecture du fichier
    val data = spark.read.textFile("///C:/Users/dell/IdeaProjects/Examen_spark/src/main/scala/deckofcards.txt")

    //2. Nettoyage
    val clean = data.filter(line=>line.split("\\|").length==3)

    //3.Transformation en Dataset
    val cards: Dataset[Card] = clean.map{ line => val Array(color, suit, value) = line.split("\\|")
    Card(color.trim, suit.trim, value.trim)}
    println("CARD: ")
    cards.show(10)

    //Normalisation des valeurs
    val norm = cards.withColumn(
      "valueNum",
      when(col("value") === "J", 11)
        .when(col("value") === "Q", 12)
        .when(col("value") === "K", 13)
        .when(col("value") === "A", 14)
        .otherwise(col("value").cast("int"))
    )
    println("CARD normalisé: ")
    norm.show(15)

    // Analyses et Statistiques
    //A. Nombre	de	cartes	par	couleur

    val nbParCouleur = norm.groupBy("color")
      .count()
    println("Nombre de cartes par couleur")
    nbParCouleur.show()

    //B. Nombre	de	cartes	par	famille

    val nbParSuit = norm.groupBy("suit")
      .count()
    println("Nombre de cartes par famille")
    nbParSuit.show()

    //C. Valeur	moyenne	par	couleur

    val moyenneParCoueur = norm.groupBy("color")
      .agg(avg("valueNum").alias("valeur_moyenne"))
    println("Valeur moyenne par couleur")
    moyenneParCoueur.show()

    //D. Carte la	plus forte par famille

    val maxParSuit = norm.groupBy("suit")
      .agg(max("valueNum").alias("carte_max"))
    println("Carte la plus forte par famille")
    maxParSuit.show()

    //Transformations avancées

    //1. Trier	toutes	les	cartes	par	valueNum	desc	puis	suit	asc

    val trie = norm.orderBy(col("valueNum").desc, col("suit").asc)
    println("Trie des cartes par valueNum desc puis suit asc")
    trie.show(10)

    //2. Export	en	JSON
/*
    val outputPath = "C:/temp/deck_output"

    trie.coalesce(1)
      .write
      .mode("overwrite")
      .json(outputPath)

    println(s"Export JSON terminé dans le dossier : $outputPath")

 */

    //3. DataFrame	groupé	par	suit	:	liste	des	valeurs	+	somme	valueNum

    val groupSuit = norm.groupBy("suit")
      .agg(
        collect_list("value").alias("liste_valeurs"),
        sum("valueNum").alias("somme_valueNum")
      )
    println("DataFrame groupé par suit")
    groupSuit.show(truncate = false)


    //Partie creative

    // Tirage aléatoire de 5 cartes (sans remise)
    val mainAleatoire = norm.orderBy(rand()).limit(5)
    println("Voici votre main aléatoire de 5 cartes :")
    mainAleatoire.show(truncate = false)




    spark.stop()
  }
}
