# ---
# "Analiza asocjacji oraz sentymentu recenzji hoteli"
#autorki: "Maria Zukowska, Emma Panasiuk, Lena Skrzypiec"
#Celem projektu jest analiza z dwoch poznanych na zajeciach metod: 
# po pierwsze analiza asocjacji, która bezpośrednio ukaże najsliniej związane z danym zagadanieniem odczucia klientow
# po drugie analiza na podstawie sentymentu i typowych slow związanych z kluczowymi w hotelarstwie usługami
#projekt pozwala na analize zarowno w sposob bezposredni poprzez asocjacje, jak rowniez podejscie od strony nacechowania emocjonalnego slow
# aby ulatwic analize osob zainteresowanych dodane są wykresy które "podsumowują" analizę sentymentu wg slownika
# Źródła: Materiały z zajęć, narzędzie Gemini do pomocy znalezieniu odpowiednich funkcji/poprawy struktury kodu

knitr::opts_chunk$set(
  message = FALSE,
  warning = FALSE
)

# --- Wymagane pakiety: ---

library(tm)
library(tidyverse)
library(tidytext)
library(wordcloud)
library(ggplot2)
library(ggthemes)
library(textdata)


# --- Dane tekstowe ----

# Wczytanie danych z pliku CSV do środowiska R i przekształcenie ich w format korpusu dokumentów.
# Założenie: plik CSV zawiera tylko jedną kolumnę tekstową z recenzjami i NIE MA NAGŁÓWKA.
# Każda linia w pliku CSV jest traktowana jako jedna, odrębna recenzja.

data <- read.table("recenzje_hilton.csv",
                   header = FALSE,# pierwsza linia to dane
                   sep = "\n",# Ustawienie separatora na znak nowej linii
                   quote = "",# recenzje będą interpretowane w całości, np " 'pretty' good" nie spowoduje rozdzielenia do nowej recenzji
                   comment.char = "",
                   stringsAsFactors = FALSE, # Zapewnia, że tekst jest wczytywany jako znak (string)\
                   encoding = "UTF-8" # Określa kodowanie pliku
)

# Po wczytaniu pliku bez nagłówka, domyślnie kolumna z danymi nazywa się 'V1'.

colnames(data) <- "Review_Text"

# Utworzenie korpusu dokumentów z wczytanych recenzji.
corpus <- VCorpus(VectorSource(data$Review_Text))


# --- 1. Przetwarzanie i oczyszczanie tekstu ----

# Zapewnienie spójnego kodowania w całym korpusie.

corpus <- tm_map(corpus, content_transformer(function(x) iconv(x, to = "UTF-8", sub = " ")))

# Funkcja pomocnicza 'toSpace' do zamiany określonych wzorców znaków na spację.
toSpace <- content_transformer(function (x, pattern) gsub(pattern, " ", x))

# Seria operacji usuwających zbędne znaki
corpus <- tm_map(corpus, content_transformer(function(x) {
  gsub("[^a-zA-Z ]", "", x)
})) #usuwa wszystkie znaki niebędące literami ani spacjami
corpus <- tm_map(corpus, toSpace, "[ \t]{2,}") # Zamiana wielokrotnych spacji/tabulacji na pojedynczą spację.

# Operacje standaryzujące tekst:
corpus <- tm_map(corpus, content_transformer(tolower)) # Konwertuje wszystkie litery na małe
corpus <- tm_map(corpus, removeWords, stopwords("english")) # Usuwa tzw. "stop words"
corpus <- tm_map(corpus, stripWhitespace) # Usuwa nadmiarowe białe znaki

# Usuwamy nazwy własne oraz specyficzne wyrażenia i zbyt ogólne pojęcia.
corpus <- tm_map(corpus, removeWords, c("nazwa_hotelu","hotel", "guests", "place", "NYC", "New York"))
corpus <- tm_map(corpus, stripWhitespace) # Ponowne usunięcie nadmiarowych spacji po usunięciu słów.


# --- Tokenizacja i Macierz Częstości Słów (TDM) ----


# Tworzenie Macierzy Częstości Słów
# Każdy wiersz reprezentuje unikalne słowo, każda kolumna to jedna recenzja,

tdm <- TermDocumentMatrix(corpus)

# Konwersja TDM na zwykłą macierz
tdm_m <- as.matrix(tdm)

# Tworzenie ramki danych z częstościami słów.
# Sumujemy wystąpienia każdego słowa we wszystkich recenzjach i sortujemy malejąco.
v <- sort(rowSums(tdm_m), decreasing = TRUE)
tdm_df <- data.frame(word = names(v), freq = v)


# --- Przygotowanie danych do analizy sentymentu dla całego korpusu (tokenizacja) ----

# Tworzenie ramki danych 'reviews_df', która będzie podstawą do tokenizacji.
reviews_df <- data.frame(
  document = seq_along(data$Review_Text), # Tworzy sekwencję numerów dla każdego dokumentu.
  text = data$Review_Text,
  stringsAsFactors = FALSE
)

# Tokenizacja recenzji na pojedyncze słowa przy użyciu 'unnest_tokens'.
# Następnie usuwamy ogólne stop-words
tidy_reviews <- reviews_df %>%
  unnest_tokens(word, text) %>% # Dzieli tekst na pojedyncze słowa i tworzy nową kolumnę 'word'.
  anti_join(stop_words) # Usuwa stop-words.


# --- Kluczowe aspekty do analizy: ----
#wybrane przez Nas aspekty do analizy hoteli:
key_aspects <- c("service", "room", "bed", "comfort", "cleanliness", "location", "wifi", "pool", "price", "experience","stay")

# Próg korelacji Pearsona dla asocjacji.
cor_limit <- 0.5 # Znaleziony zalecany przedział dla recenzji 0,4-0,6


# --- Puste listy do przechowywania wyników sentymentu ----
all_nrc_sentiment <- list()
all_bing_sentiment <- list()
all_afinn_sentiment <- list()

# --- Pętla analizy dla każdego kluczowego aspektu ----
# Pętla ta iteruje przez każdy zdefiniowany 'key_aspect' i dla każdego z nich:
# 1. Oblicza asocjacje (słowa często występujące razem z aspektem).
# 2. Wizualizuje te asocjacje.
# 3. Przeprowadza analizę sentymentu skojarzonych słów przy użyciu słowników NRC, Bing i AFINN.

for (target_word in key_aspects) {
  message(paste0("\n--- Analiza dla aspektu: '", target_word, "' ---\n"))
  
  # Obliczanie asocjacji dla bieżącego słowa
  associations <- findAssocs(tdm, target_word, corlimit = cor_limit)
  
  # Wyodrębnienie wektora asocjacji dla bieżącego słowa.
  assoc_vector <- associations[[target_word]]
  
  # Sortowanie asocjacji malejąco według współczynnika korelacji.
  assoc_sorted <- sort(assoc_vector, decreasing = TRUE)
  
  # Tworzenie ramki danych z asocjacjami.
  
  assoc_df <- data.frame(
    word = factor(names(assoc_sorted), levels = names(assoc_sorted)[order(assoc_sorted)]),
    score = assoc_sorted
  )
  
  # Wyświetlanie znalezionych asocjacji w konsoli.
  if (nrow(assoc_df) > 0) { # Sprawdza, czy znaleziono jakiekolwiek asocjacje.
    message(paste0("Asocjacje dla słowa '", target_word, "' (próg r >= ", cor_limit, "):"))
    print(assoc_df)
    
    # Wizualizacja asocjacji za pomocą wykresu słupkowego z ggplot2.
    
    print(
      ggplot(assoc_df, aes(x = score, y = reorder(word, score), color = score)) +
        geom_segment(aes(x = 0, xend = score, y = word, yend = word), size = 1.2) + # Linie od 0 do wartości korelacji.
        geom_point(size = 4) + # Punkty na końcu linii.
        geom_text(aes(label = round(score, 2)), hjust = -0.3, size = 3.5, color = "black") + # Etykiety z wartościami korelacji.
        scale_color_gradient(low = "#a6bddb", high = "#08306b") + # Gradient kolorów dla siły korelacji.
        scale_x_continuous(
          limits = c(0, max(assoc_df$score) + 0.1), # Ustawia limit osi X z małym zapasem.
          expand = expansion(mult = c(0, 0.2)) # Rozszerza oś X.
        ) +
        theme_minimal(base_size = 12) + # Używa minimalistycznego motywu.
        labs(
          title = paste0("Asocjacje z terminem: '", target_word, "'"),
          subtitle = paste0("Próg r \u2265 ", cor_limit), # Podtytuł z progiem korelacji.
          x = "Współczynnik korelacji Pearsona",
          y = "Słowo",
          color = "Natężenie\nskojarzenia" # Etykieta legendy.
        ) +
        theme(
          plot.title = element_text(face = "bold"), # Pogrubienie tytułu wykresu.
          axis.title.x = element_text(margin = margin(t = 10)), # Marginesy tytułów osi.
          axis.title.y = element_text(margin = margin(r = 10)),
          legend.position = "right" # Pozycja legendy.
        )
    )
    
    # --- Analiza sentymentu dla skorelowanych słów ----
    
    correlated_words <- as.character(assoc_df$word)
    
    # Filtrowanie tokenizowanych recenzji, aby zawierały tylko skorelowane słowa.
    tidy_correlated_tokens <- tidy_reviews %>%
      filter(word %in% correlated_words)
    
    if (nrow(tidy_correlated_tokens) > 0) { # Sprawdza, czy są jakieś skorelowane słowa do analizy sentymentu.
      message(paste0("\nAnaliza Sentymentu Skorelowanych Słów dla '", target_word, "':\n"))
      
      # Analiza sentymentu przy użyciu słownika NRC.
      # Słownik NRC przypisuje słowom emocje (np. radość, strach) oprócz pozytywnego/negatywnego sentymentu.
      sentiment_nrc_aspect <- tidy_correlated_tokens %>%
        inner_join(get_sentiments("nrc"), relationship = "many-to-many") %>% # Łączy słowa z ich sentymentem/emocjami z NRC.
        count(word, sentiment) %>%
        mutate(aspect = target_word)
      if (nrow(sentiment_nrc_aspect) > 0) {
        all_nrc_sentiment[[target_word]] <- sentiment_nrc_aspect
        nrc_counts <- sentiment_nrc_aspect %>%
          group_by(sentiment) %>%
          top_n(10, n) %>% # Wybiera 10 najczęściej występujących słów dla każdego sentymentu/emocji (NRC).
          ungroup() %>%
          mutate(word = reorder(word, n))
        print(
          ggplot(nrc_counts, aes(x = word, y = n, fill = sentiment)) +
            geom_col(show.legend = FALSE) +
            facet_wrap(~sentiment, scales = "free_y") +
            coord_flip() +
            labs(x = "Słowa", y = "Liczba",
                 title = paste0("Sentyment skorelowanych słów z '", target_word, "' (NRC)")) +
            theme_gdocs()
        )
      } else {
        message(paste0("Brak słów do analizy sentymentu NRC dla '", target_word, "'."))
      }
      
      # Analiza sentymentu przy użyciu słownika Bing.
      # Słownik Bing klasyfikuje słowa jako "pozytywne" lub "negatywne". Jest to słownik ogólnego przeznaczenia.
      sentiment_bing_aspect <- tidy_correlated_tokens %>%
        inner_join(get_sentiments("bing")) %>% # Łączy słowa z ich sentymentem z Bing.
        count(word, sentiment) %>%
        mutate(aspect = target_word)
      if (nrow(sentiment_bing_aspect) > 0) {
        all_bing_sentiment[[target_word]] <- sentiment_bing_aspect
        bing_counts <- sentiment_bing_aspect %>%
          group_by(sentiment) %>%
          top_n(10, n) %>% # Wybiera 10 najczęściej występujących słów dla każdego sentymentu (Bing).
          ungroup() %>%
          mutate(word = reorder(word, n))
        print(
          ggplot(bing_counts, aes(x = word, y = n, fill = sentiment)) +
            geom_col(show.legend = FALSE) +
            facet_wrap(~sentiment, scales = "free_y") +
            coord_flip() +
            labs(x = "Słowa", y = "Liczba",
                 title = paste0("Sentyment skorelowanych słów z '", target_word, "' (Bing)")) +
            theme_gdocs()
        )
        
        # Dodatkowe podsumowanie dla danego aspektu (Bing).
        total_positive_bing <- bing_counts %>% filter(sentiment == "positive") %>% pull(n) %>% sum()
        total_negative_bing <- bing_counts %>% filter(sentiment == "negative") %>% pull(n) %>% sum()
        
        message(paste0("\n--- Podsumowanie sentymentu dla aspektu '", target_word, "' (Bing): ---"))
        if (total_positive_bing > total_negative_bing) {
          message(paste0("Więcej pozytywnych słów skorelowanych:", total_positive_bing, " (pozytywne) vs ", total_negative_bing, " (negatywne). Sentyment dla '", target_word, "' jest raczej pozytywny."))
        } else if (total_negative_bing > total_positive_bing) {
          message(paste0("Więcej negatywnych słów skorelowanych:", total_negative_bing, "(negatywne) vs ", total_positive_bing, " (pozytywne). Sentyment dla '", target_word, "' jest raczej negatywny."))
        } else if (total_positive_bing > 0) {
          message(paste0("Równowaga między pozytywnymi i negatywnymi słowami skorelowanymi (", total_positive_bing, " pozytywnych, ", total_negative_bing, " negatywnych). Sentyment dla '", target_word, "' jest neutralny/zrównoważony."))
        } else {
          message("Brak słów o określonym sentymencie (pozytywnym/negatywnym) skorelowanych z tym aspektem. Brak jasnego sentymentu.")
        }
        message("--------------------------------------------------")
        
      } else {
        message(paste0("Brak słów do analizy sentymentu Bing dla '", target_word, "'."))
      }
      
      # Analiza sentymentu przy użyciu słownika AFINN.
      # Słownik AFINN przypisuje słowom wartości numeryczne od -5 (bardzo negatywny) do +5 (bardzo pozytywny).
      sentiment_afinn_aspect <- tidy_correlated_tokens %>%
        inner_join(get_sentiments("afinn")) %>% # Łączy słowa z ich wartościami sentymentu z AFINN.
        group_by(word) %>% # Grupujemy po słowie, aby zsumować ich wartości, jeśli występują wiele razy
        summarise(total_value = sum(value), n_occurrences = n()) %>% # Zlicza wystąpienia słowa i sumuje wartości sentymentu
        mutate(aspect = target_word) %>%
        arrange(desc(abs(total_value))) # Sortuje według wartości bezwzględnej dla ważności
      
      if (nrow(sentiment_afinn_aspect) > 0) {
        all_afinn_sentiment[[target_word]] <- sentiment_afinn_aspect
        
        # Klasyfikacja słów na pozytywne/negatywne/neutralne dla wizualizacji AFINN
        afinn_plot_data <- sentiment_afinn_aspect %>%
          mutate(sentiment_category = case_when(
            total_value > 0 ~ "positive",
            total_value < 0 ~ "negative",
            TRUE ~ "neutral"
          )) %>%
          # Dla przejrzystości, wybieramy Top 10 najbardziej nacechowanych słów
          group_by(sentiment_category) %>%
          top_n(10, abs(total_value)) %>%
          ungroup() %>%
          mutate(word = reorder(word, total_value)) # Uporządkowanie słów
        
        # Wykres sentymentu AFINN
        print(
          ggplot(afinn_plot_data, aes(x = word, y = total_value, fill = sentiment_category)) +
            geom_col(show.legend = FALSE) +
            facet_wrap(~sentiment_category, scales = "free_y") +
            coord_flip() +
            labs(x = "Słowa", y = "Wynik Sentymentu (AFINN)",
                 title = paste0("Sentyment skorelowanych słów z '", target_word, "' (AFINN)")) +
            theme_gdocs() +
            scale_fill_manual(values = c("negative" = "firebrick", "positive" = "darkolivegreen4", "neutral" = "gray"))
        )
        
        # Podsumowanie dla AFINN
        total_score_afinn <- sum(sentiment_afinn_aspect$total_value)
        message(paste0("\n--- Podsumowanie sentymentu dla aspektu '", target_word, "' (AFINN): ---"))
        if (total_score_afinn > 0) {
          message(paste0("Całkowity wynik sentymentu (AFINN): ", total_score_afinn, ". Sentyment dla '", target_word, "' jest raczej pozytywny."))
        } else if (total_score_afinn < 0) {
          message(paste0("Całkowity wynik sentymentu (AFINN): ", total_score_afinn, ". Sentyment dla '", target_word, "' jest raczej negatywny."))
        } else {
          message("Całkowity wynik sentymentu (AFINN): 0. Sentyment dla '", target_word, "' jest neutralny/zrównoważony.")
        }
        message("--------------------------------------------------")
        
      } else {
        message(paste0("Brak słów do analizy sentymentu AFINN dla '", target_word, "'."))
      }
      
    } else {
      message(paste0("Brak skorelowanych tokenów do analizy sentymentu dla '", target_word, "'."))
    }
  } else {
    message(paste0("Brak asocjacji dla słowa '", target_word, "' przy progu r >= ", cor_limit, "."))
  }
}

# --- Ogólna analiza sentymentu dla całego korpusu ----
# Ta sekcja podsumowuje ogólny sentyment wszystkich recenzji, bez podziału na aspekty.
message("\n--- Ogólna Analiza Sentymentu Całego Korpusu ---\n")

# Ogólna analiza sentymentu przy użyciu słownika Bing (binarny: pozytywny/negatywny)
overall_sentiment_bing <- tidy_reviews %>%
  inner_join(get_sentiments("bing")) %>%
  count(sentiment) %>% # Zlicza słowa pozytywne i negatywne.
  mutate(percentage = n / sum(n) * 100) # Oblicza udział procentowy.

print(overall_sentiment_bing)

# Wizualizacja ogólnego sentymentu Bing za pomocą wykresu kołowego.
ggplot(overall_sentiment_bing, aes(x = "", y = n, fill = sentiment)) +
  geom_bar(width = 1, stat = "identity") + # Tworzy słupki dla wykresu kołowego.
  coord_polar("y", start = 0) + # Przekształca słupki w wykres kołowy.
  labs(title = "Ogólny Sentyment w Recenzjach (Bing)", fill = "Sentyment") + # Tytuł i legenda.
  theme_void() + # Usuwa zbędne elementy tła.
  scale_fill_manual(values = c("negative" = "firebrick", "positive" = "darkolivegreen4")) + # Ręczne ustawienie kolorów.
  geom_text(aes(label = paste0(round(percentage, 1), "%")), # Dodaje etykiety procentowe na wykresie.
            position = position_stack(vjust = 0.5), color = "white", size = 5)

# Podsumowanie tekstowe dla ogólnego sentymentu Bing
if ("positive" %in% overall_sentiment_bing$sentiment && "negative" %in% overall_sentiment_bing$sentiment) {
  positive_percentage <- overall_sentiment_bing %>% filter(sentiment == "positive") %>% pull(percentage)
  negative_percentage <- overall_sentiment_bing %>% filter(sentiment == "negative") %>% pull(percentage)
  
  if (positive_percentage > negative_percentage) {
    message(paste0("\nOgólny sentyment w recenzjach (Bing) jest bardziej pozytywny (", round(positive_percentage, 1), "%) niż negatywny (", round(negative_percentage, 1), "%)."))
  } else if (negative_percentage > positive_percentage) {
    message(paste0("\nOgólny sentyment w recenzjach (Bing) jest bardziej negatywny (", round(negative_percentage, 1), "%) niż pozytywny (", round(positive_percentage, 1), "%)."))
  } else {
    message("\nOgólny sentyment w recenzjach (Bing) jest zrównoważony.")
  }
} else if ("positive" %in% overall_sentiment_bing$sentiment) {
  message("\nW recenzjach (Bing) dominują słowa o sentymencie pozytywnym.")
} else if ("negative" %in% overall_sentiment_bing$sentiment) {
  message("\nW recenzjach (Bing) dominują słowa o sentymencie negatywnym.")
} else {
  message("\nBrak słów o pozytywnym lub negatywnym sentymencie w całym korpusie według słownika Bing.")
}

# --- Ogólna analiza sentymentu dla całego korpusu (AFINN) ---
# Sumowanie wyników AFINN dla całego korpusu
overall_sentiment_afinn_raw <- tidy_reviews %>%
  inner_join(get_sentiments("afinn"))

# Tworzenie danych do wizualizacji dla AFINN (Top N pozytywnych i negatywnych)
overall_afinn_plot_data <- overall_sentiment_afinn_raw %>%
  group_by(word) %>%
  summarise(total_value = sum(value), n_occurrences = n()) %>%
  mutate(sentiment_category = case_when(
    total_value > 0 ~ "positive",
    total_value < 0 ~ "negative",
    TRUE ~ "neutral" # W AFINN neutralne (0) słowa są rzadkie, ale dla kompletności
  )) %>%
  # Wybierz top N pozytywnych i negatywnych słów do wizualizacji
  group_by(sentiment_category) %>%
  top_n(10, abs(total_value)) %>% # Wybiera 10 słów o największej bezwzględnej wartości sentymentu
  ungroup() %>%
  mutate(word = reorder(word, total_value)) # Uporządkowanie słów dla lepszej czytelności wykresu

# Wizualizacja ogólnego sentymentu AFINN (Top N słów)
if (nrow(overall_afinn_plot_data) > 0) {
  print(
    ggplot(overall_afinn_plot_data, aes(x = word, y = total_value, fill = sentiment_category)) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~sentiment_category, scales = "free_y") +
      coord_flip() +
      labs(x = "Słowa", y = "Wynik Sentymentu (AFINN)",
           title = "Ogólny Sentyment w Recenzjach (AFINN) - Top słowa") +
      theme_gdocs() +
      scale_fill_manual(values = c("negative" = "firebrick", "positive" = "darkolivegreen4", "neutral" = "gray")) +
      geom_text(aes(label = total_value), hjust = ifelse(overall_afinn_plot_data$total_value > 0, -0.2, 1.2), size = 3.5, color = "black")
  )
} else {
  message("Brak słów o określonym sentymencie w całym korpusie według słownika AFINN do wizualizacji.")
}

# Podsumowanie tekstowe dla ogólnego sentymentu AFINN
total_score_afinn_overall <- sum(overall_sentiment_afinn_raw$value)
message(paste0("\n--- Ogólny Sentyment Całego Korpusu (AFINN): ---"))
message(paste0("Całkowity wynik sentymentu AFINN dla całego korpusu:", total_score_afinn_overall, "."))

if (total_score_afinn_overall > 0) {
  message("Ogólny sentyment w recenzjach (AFINN) jest pozytywny.")
} else if (total_score_afinn_overall < 0) {
  message("Ogólny sentyment w recenzjach (AFINN) jest negatywny.")
} else {
  message("Ogólny sentyment w recenzjach (AFINN) jest neutralny/zrównoważony.")
}
message("--------------------------------------------------")

# --- Ogólna analiza sentymentu dla całego korpusu (NRC) ---
message("\n--- Ogólna Analiza Sentymentu Całego Korpusu (NRC) ---")

# 1. Pobierz wszystkie unikalne kategorie sentymentu/emocji ze słownika NRC.
# To jest kluczowe, aby zapewnić, że wizualizacja uwzględnia wszystkie możliwe kategorie,
# nawet jeśli dla niektórych nie ma słów w recenzjach.
all_nrc_sentiments <- get_sentiments("nrc")$sentiment %>% unique()

# 2. Przeprowadź analizę sentymentu NRC dla całego korpusu:
#    - Połącz tokenizowane recenzje z danymi sentymentu NRC.
#    - Użyj relationship = "many-to-many" aby zezwolić na wielokrotne przypisania sentymentu do słowa.
#    - Zlicz wystąpienia każdego sentymentu.
#    - Użyj right_join, aby dodać kategorie sentymentu, które nie wystąpiły w recenzjach (zliczenie = 0).
#    - Zastąp wartości NA (z powodu braku wystąpień) zerami.
#    - Oblicz udział procentowy każdego sentymentu.
#    - Jawnie przekształć kolumnę 'sentiment' na faktor z predefiniowanymi poziomami.
#      Jest to bardzo ważne dla prawidłowego działania ggplot2 i scale_fill_manual.
overall_sentiment_nrc <- tidy_reviews %>%
  inner_join(get_sentiments("nrc"), relationship = "many-to-many") %>%
  count(sentiment) %>%
  right_join(data.frame(sentiment = all_nrc_sentiments), by = "sentiment") %>%
  mutate(n = replace_na(n, 0)) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  mutate(sentiment = factor(sentiment, levels = c(
    "anger", "anticipation", "disgust", "fear", "joy",
    "negative", "positive", "sadness", "surprise", "trust"
  )))

# Wyświetl ramkę danych z wynikami analizy NRC
print(overall_sentiment_nrc)

# 3. Wizualizacja ogólnego sentymentu NRC
#    - Użyj ggplot2 do stworzenia wykresu słupkowego.
#    - Sentymenty zostaną posortowane według liczby wystąpień (n).
#    - Kolumny będą pokolorowane zgodnie z kategorią sentymentu (fill = sentiment).
#    - Użyj coord_flip() dla wykresu słupkowego poziomego.
#    - Dodaj tytuły i etykiety osi.
#    - Zastosuj motyw ggthemes::theme_gdocs().
#    - Ręcznie zdefiniuj kolory dla każdej kategorii sentymentu NRC,
#      zapewniając spójność z wcześniej zdefiniowanymi poziomami faktora.
#    - Dodaj etykiety procentowe na słupkach.
ggplot(overall_sentiment_nrc, aes(x = reorder(sentiment, n), y = n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(x = "Sentyment/Emocja", y = "Liczba słów",
       title = "Ogólny Sentyment i Emocje w Recenzjach (NRC)") +
  theme_gdocs() +
  scale_fill_manual(values = c(
    "anger" = "#e41a1c", "anticipation" = "#377eb8", "disgust" = "#4daf4a",
    "fear" = "#984ea3", "joy" = "#ff7f00", "negative" = "#a65628",
    "positive" = "#f781bf", "sadness" = "#999999", "surprise" = "#dede00",
    "trust" = "#66c2a5"
  )) +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), hjust = -0.1, size = 3.5)

# 4. Podsumowanie tekstowe ogólnego sentymentu i emocji (NRC)
message("\n--- Podsumowanie ogólnego sentymentu i emocji (NRC): ---")

if ("positive" %in% levels(overall_sentiment_nrc$sentiment) &&
    "negative" %in% levels(overall_sentiment_nrc$sentiment)) {
  
  positive_n <- overall_sentiment_nrc %>% filter(sentiment == "positive") %>% pull(n)
  negative_n <- overall_sentiment_nrc %>% filter(sentiment == "negative") %>% pull(n)
  
  message(paste0("Liczba słów pozytywnych (NRC): ", positive_n))
  message(paste0("Liczba słów negatywnych (NRC): ", negative_n))
  
  if (positive_n > negative_n) {
    message("Ogólny sentyment NRC jest bardziej pozytywny.")
    
    
  } else if (negative_n > positive_n) {
    message("Ogólny sentyment NRC jest bardziej negatywny.")
  } else {
    message("Ogólny sentyment NRC jest zrównoważony.")
  }
} else {
  message("Brak danych dla sentymentu pozytywnego lub negatywnego w NRC.")
}
message("--------------------------------------------------")

