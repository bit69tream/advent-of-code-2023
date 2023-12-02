#include <stdlib.h>
#include <assert.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <ctype.h>

char *read_whole_file(const char *path) {
#define MAX_BUFFER (64 * 1024 * 1024)
  static char buffer[MAX_BUFFER] = {0};
  memset(buffer, 0, sizeof(buffer));

  FILE *fp = fopen(path, "r");
  assert(fp);

  fread(buffer, sizeof(buffer), 1, fp);

  fclose(fp);

  return buffer;
}

typedef struct {
  int red;
  int green;
  int blue;
} Set;

#define MAX_SETS_PER_GAME 64

typedef struct {
  Set sets[MAX_SETS_PER_GAME];
  size_t sets_len;
} Game;

#define MAX_GAMES 128

typedef struct {
  char * data;
  size_t len;
} String;

ssize_t find(String data, char target) {
  for (size_t i = 0; i < data.len; i++) {
    if (data.data[i] == target) {
      return i;
    }
  }

  return -1;
}

size_t split_into_lines(String data, String *lines, size_t lines_max) {
  size_t lines_len = 0;
  while (true) {
    ssize_t newline = find(data, '\n');
    if (newline == -1) {
      return lines_len;
    }

    lines[lines_len] = (String) {
      .data = data.data,
      .len = (size_t) newline,
    };

    if ((lines_len + 1) == lines_max) {
      return lines_len;
    }

    lines_len += 1;

    data.data += newline + 1;
    data.len -= newline + 1;
  }

  return lines_len;
}

void trim_left(String *str) {
  while (str->len > 0 &&
         (isspace(str->data[0]) ||
          str->data[0] == ',')) {
    str->data += 1;
    str->len -= 1;
  }
}

String *split(String string, char by, size_t *split_len) {
  static String result[MAX_SETS_PER_GAME];

  *split_len = 0;
  size_t i = 0;
  size_t last_i = 0;
  for (i = 0; i < string.len; i++) {
    if (string.data[i] == by) {
      result[*split_len].data = string.data + last_i;
      result[*split_len].len = (i) - last_i;
      *split_len += 1;
      last_i = i + 1;
    }
  }

  result[*split_len].data = string.data + last_i;
  result[*split_len].len = (i) - last_i;
  *split_len += 1;

  return result;
}

Game *parse_games(char * raw_input, size_t *games_len) {
  static String lines[MAX_GAMES];

  String input = (String) {
    .data = raw_input,
    .len = strlen(raw_input),
  };

  size_t lines_len = split_into_lines(input, lines, MAX_GAMES);

  static Game games[MAX_GAMES];
  for (size_t i = 0; i < lines_len; i++) {
    char * sets_begin = strchr(lines[i].data, ':');
    assert(sets_begin);

    lines[i].len -= (sets_begin - lines[i].data) + 1;
    lines[i].data = sets_begin + 1;

    size_t sets_len = 0;
    String *sets = split(lines[i], ';', &sets_len);

    games[i].sets_len = 0;
    for (size_t j = 0; j < sets_len; j++) {
      Set set = {0};
      while (sets[j].len > 0) {
        trim_left(&sets[j]);

        char *endptr = NULL;
        long num = strtol(sets[j].data, &endptr, 10);

        sets[j].len -= endptr - sets[j].data;
        sets[j].data = endptr;
        trim_left(&sets[j]);

        size_t x = 0;
        if (strncmp(sets[j].data, "blue", 4) == 0) {
          set.blue = num;
          x = 4;
        } else if (strncmp(sets[j].data, "red", 3) == 0) {
          set.red = num;
          x = 3;
        } else if (strncmp(sets[j].data, "green", 5) == 0) {
          set.green = num;
          x = 5;
        } else {
          assert(false);
        }

        if (sets[j].len == x) {
          break;
        }

        sets[j].len -= x;
        sets[j].data += x;
      }
      games[i].sets[games[i].sets_len++] = set;
    }
    *games_len = i;
  }

  *games_len += 1;
  return games;
}

#define MAX_RED 12
#define MAX_GREEN 13
#define MAX_BLUE 14
bool is_game_possible(const Game *game) {
  for (size_t i = 0; i < game->sets_len; i++) {
    if (game->sets[i].red > MAX_RED ||
        game->sets[i].green > MAX_GREEN ||
        game->sets[i].blue > MAX_BLUE) {
      return false;
    }
  }

  return true;
}

size_t power(const Game *game) {
  int max_red = 0;
  int max_green = 0;
  int max_blue = 0;

#define MAX(a, b) (a > b ? a : b)

  for (size_t i = 0; i < game->sets_len; i++) {
    max_red = MAX(game->sets[i].red, max_red);
    max_green = MAX(game->sets[i].green, max_green);
    max_blue = MAX(game->sets[i].blue, max_blue);
  }

  return max_red * max_green * max_blue;
}

int main(int argc, char **argv) {
  if (argc != 2) {
    fprintf(stderr, "usage: day2 input.txt\n");
    return 1;
  }
  printf("using %s\n", argv[1]);

  char * input = read_whole_file(argv[1]);

  size_t games_len = 0;
  Game *games = parse_games(input, &games_len);

  {
    size_t sum = 0;
    for (size_t i = 0; i < games_len; i++) {
      if (is_game_possible(&games[i])) {
        sum += i + 1;
      }
    }

    printf("part 1: %lu\n", sum);
  }


  {
    size_t sum = 0;
    for (size_t i = 0; i < games_len; i++) {
      sum += power(&games[i]);
    }

    printf("part 2: %lu\n", sum);
  }
  return 0;
}
