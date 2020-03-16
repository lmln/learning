/*
   Compile with:
   gcc -shared -fPIC fmatrices.c -o fmatrices.o -O3

   Or:
   cl -c -DWIN32 libcmatrices.c /O2
   link -dll -out:libcmatrices.so libcmatrices.obj
*/

#ifdef WIN32
#define EXPORT extern __declspec (dllexport)
#include <windows.h>
#else
#define EXPORT extern
#endif

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <time.h>

#define _USE_MATH_DEFINES
#include <math.h>

int is_srand_initialized = 0;
long long number_of_matrices = 0;

struct matrix
{
  double * nums;
  size_t rows;
  size_t columns;
};

struct point
{
  size_t row;
  size_t column;
  double value;
};

EXPORT struct matrix *
make_matrix(size_t rows, size_t columns, double value)
{
  size_t s = rows * columns;

  struct matrix * new_matrix = malloc(sizeof(struct matrix));
  new_matrix->nums = malloc(sizeof(double) * s);
  new_matrix->rows = rows;
  new_matrix->columns = columns;

  for (size_t i = 0; i < s; i++)
    new_matrix->nums[i] = value;

  number_of_matrices++;

  return new_matrix;
}

int did_i_just = 1;

EXPORT void
free_matrix(struct matrix * matrix)
{
  free(matrix->nums);
  free(matrix);
  /*
     if (number_of_matrices % 1000 == 0)
     printf("Number of matrices: %llu\n", number_of_matrices);
     number_of_matrices--;
  */

}

EXPORT double
box_muller()
{
  if (!is_srand_initialized)
    {
      srand(time(NULL));
      is_srand_initialized = 1;
    }

  double u1 = (double) rand() / RAND_MAX;
  double u2 = (double) rand() / RAND_MAX;

  if (u1 == 0 || u2 == 0) {
    return box_muller();
  }

  double R = sqrt(-2.0 * log(u1));
  double theta = 2 * M_PI * u2;

  double ret = (cos(theta) * R);

  return (cos(theta) * R);
}

EXPORT struct matrix *
make_random_matrix(size_t rows, size_t columns)
{
  size_t s = rows * columns;

  struct matrix * new_matrix = malloc(sizeof(struct matrix));
  new_matrix->nums = malloc(sizeof(double) * s);
  new_matrix->rows = rows;
  new_matrix->columns = columns;

  for (size_t i = 0; i < s; i++)
    new_matrix->nums[i] = box_muller();

  return new_matrix;
}

EXPORT void
matrix_set(struct matrix * matrix, size_t i, size_t j, double value)
{
  matrix->nums[i * matrix->columns + j] = value;
  /* return matrix;  */
}

EXPORT double
matrix_ref(struct matrix * matrix, size_t i, size_t j)
{
  return matrix->nums[i * matrix->columns + j];
}

EXPORT void
display_matrix(struct matrix * matrix)
{
  size_t s = matrix->rows * matrix->columns;

  printf("#(");

  for (size_t i = 0; i < matrix->rows; i++)
    {
      printf("#(");
      for (size_t j = 0; j < matrix->columns; j++)
        {
          if (j == (matrix->columns - 1))
            printf("%Lf", matrix->nums[i * matrix->columns + j]);
          else
            printf("%Lf ", matrix->nums[i * matrix->columns + j]);
        }
      printf(")");
    }
  printf(")\n");
}

EXPORT struct matrix *
transpose(struct matrix * oldmatrix)
{
  size_t s = oldmatrix->rows * oldmatrix->columns;

  struct matrix * new_matrix = malloc(sizeof(struct matrix));
  new_matrix->nums = malloc(sizeof(double) * s);

  new_matrix->rows = oldmatrix->columns;
  new_matrix->columns = oldmatrix->rows;

  for (size_t i = 0; i < new_matrix->rows; i++)
    for (size_t j = 0; j < new_matrix->columns; j++)
      new_matrix->nums[i * new_matrix->columns + j] = oldmatrix->nums[j * oldmatrix->columns + i];

  return new_matrix;
}

EXPORT struct matrix *
hadamard(struct matrix * mmatrix, struct matrix * kmatrix)
{
  size_t s = mmatrix->rows * mmatrix->columns;

  struct matrix * new_matrix = malloc(sizeof(struct matrix));
  new_matrix->nums = malloc(sizeof(double) * s);

  new_matrix->rows = mmatrix->rows;
  new_matrix->columns = mmatrix->columns;


  for (size_t i = 0; i < s; i++)
    new_matrix->nums[i] = mmatrix->nums[i] * kmatrix->nums[i];

  return new_matrix;
}

EXPORT struct matrix *
destructive_hadamard(struct matrix * mmatrix, struct matrix * kmatrix)
{
  size_t s = mmatrix->rows * mmatrix->columns;

  for (size_t i = 0; i < s; i++)
    kmatrix->nums[i] = mmatrix->nums[i] * kmatrix->nums[i];

  return kmatrix;
}

EXPORT struct matrix *
scale(double scalar, struct matrix * mmatrix)
{
  size_t s = mmatrix->rows * mmatrix->columns;

  struct matrix * new_matrix = malloc(sizeof(struct matrix));
  new_matrix->nums = malloc(sizeof(double) * s);

  new_matrix->rows = mmatrix->rows;
  new_matrix->columns = mmatrix->columns;

  for (size_t i = 0; i < s; i++)
    new_matrix->nums[i] = scalar * mmatrix->nums[i];

  return new_matrix;
}

EXPORT struct matrix *
destructive_scale(double scalar, struct matrix * mmatrix)
{
  size_t s = mmatrix->rows * mmatrix->columns;

  for (size_t i = 0; i < s; i++)
    mmatrix->nums[i] = scalar * mmatrix->nums[i];

  return mmatrix;
}


EXPORT struct matrix *
sigmoid(struct matrix * mmatrix)
{
  size_t s = mmatrix->rows * mmatrix->columns;

  struct matrix * new_matrix = malloc(sizeof(struct matrix));
  new_matrix->nums = malloc(sizeof(double) * s);

  new_matrix->rows = mmatrix->rows;
  new_matrix->columns = mmatrix->columns;

  for (size_t i = 0; i < s; i++)
    new_matrix->nums[i] = 1 / (1 + exp(0 - mmatrix->nums[i]));

  return new_matrix;
}

EXPORT struct matrix *
destructive_sigmoid(struct matrix * mmatrix)
{
  size_t s = mmatrix->rows * mmatrix->columns;

  for (size_t i = 0; i < s; i++)
    mmatrix->nums[i] = 1 / (1 + exp(0 - mmatrix->nums[i]));

  return mmatrix;
}

EXPORT struct matrix *
sigmoid_derivative(struct matrix * mmatrix)
{
  size_t s = mmatrix->rows * mmatrix->columns;

  struct matrix * new_matrix = malloc(sizeof(struct matrix));
  new_matrix->nums = malloc(sizeof(double) * s);

  new_matrix->rows = mmatrix->rows;
  new_matrix->columns = mmatrix->columns;

  double x = 0.0;

  for (size_t i = 0; i < s; i++)
    {
      x = 1 / (1 + exp(0 - mmatrix->nums[i]));
      new_matrix->nums[i] = x * (1 - x);
    }

  return new_matrix;
}

EXPORT struct matrix *
destructive_sigmoid_derivative(struct matrix * mmatrix)
{
  size_t s = mmatrix->rows * mmatrix->columns;

  double x = 0.0;

  for (size_t i = 0; i < s; i++)
    {
      x = 1 / (1 + exp(0 - mmatrix->nums[i]));
      mmatrix->nums[i] = x * (1 - x);
    }

  return mmatrix;
}

EXPORT struct matrix *
mul(struct matrix * mmatrix, struct matrix * kmatrix)
{
  /* (i * m + j) */

  assert(mmatrix->columns == kmatrix->rows);

  size_t s  = mmatrix->rows * kmatrix->columns;
  size_t mr = mmatrix->rows;
  size_t mc = mmatrix->columns;
  size_t kr = kmatrix->rows;
  size_t kc = kmatrix->columns;

  struct matrix * new_matrix = malloc(sizeof(struct matrix));
  new_matrix->nums = malloc(sizeof(double) * s);

  new_matrix->rows = mmatrix->rows;
  new_matrix->columns = kmatrix->columns;

  for (size_t i = 0; i < mr; i++)
    {
      for (size_t j = 0; j < kc; j++)
        {
          double sum = 0;
          for (size_t y = 0; y < kr; y++)
            sum = sum + (matrix_ref(mmatrix, i, y) * matrix_ref(kmatrix, y, j));

          new_matrix->nums[i * new_matrix->columns + j] = sum;
          /* matrix_set(new_matrix, i, j, sum); */
        }
    }
  number_of_matrices++;
  return new_matrix;
}

EXPORT struct matrix *
sum(struct matrix * mmatrix, struct matrix * kmatrix)
{
  size_t s = mmatrix->rows * mmatrix->columns;

  struct matrix * new_matrix = malloc(sizeof(struct matrix));
  new_matrix->nums = malloc(sizeof(double) * s);

  new_matrix->rows = mmatrix->rows;
  new_matrix->columns = mmatrix->columns;

  for (size_t i = 0; i < s; i++)
    {
      new_matrix->nums[i] = mmatrix->nums[i] + kmatrix->nums[i];
    }

  number_of_matrices++;
  return new_matrix;
}

EXPORT struct matrix *
destructive_sum(struct matrix * mmatrix, struct matrix * kmatrix)
{
  size_t s = mmatrix->rows * mmatrix->columns;

  for (size_t i = 0; i < s; i++)
    kmatrix->nums[i] = mmatrix->nums[i] + kmatrix->nums[i];

  return kmatrix;
}

EXPORT struct matrix *
dif(struct matrix * mmatrix, struct matrix * kmatrix)
{
  size_t s = mmatrix->rows * mmatrix->columns;

  struct matrix * new_matrix = malloc(sizeof(struct matrix));
  new_matrix->nums = malloc(sizeof(double) * s);

  new_matrix->rows = mmatrix->rows;
  new_matrix->columns = mmatrix->columns;

  for (size_t i = 0; i < s; i++)
    new_matrix->nums[i] = mmatrix->nums[i] - kmatrix->nums[i];

  return new_matrix;
}

EXPORT struct matrix *
destructive_dif(struct matrix * mmatrix, struct matrix * kmatrix)
{
  size_t s = mmatrix->rows * mmatrix->columns;

  for (size_t i = 0; i < s; i++)
    kmatrix->nums[i] = mmatrix->nums[i] - kmatrix->nums[i];

  return kmatrix;
}

EXPORT size_t
matrix_rows(struct matrix * matrix)
{
  return matrix->rows;
}

EXPORT size_t
matrix_columns(struct matrix * matrix)
{
  return matrix->columns;
}

EXPORT struct point
matrix_max_pos(struct matrix * matrix)
{
  struct point new_point;
  new_point.row = 0, new_point.column = 0;
  new_point.value = matrix_ref(matrix, 0, 0);
  size_t size = matrix->rows * matrix->columns;
  double seen_value = 0.0;
  for (size_t i = 0; i < matrix->rows; i++)
    for (size_t j = 0; j < matrix->columns; j++)
      {
        seen_value = matrix->nums[i * matrix->columns + j];
        if (seen_value > new_point.value)
          {
            new_point.row = i;
            new_point.column = j;
            new_point.value = seen_value;
          }
      }

  return new_point;
}

EXPORT size_t
matrix_max_row(struct matrix * matrix)
{
  struct point max_point = matrix_max_pos(matrix);
  return max_point.row;
}

EXPORT size_t
matrix_max_column(struct matrix * matrix)
{
  struct point max_point = matrix_max_pos(matrix);
  return max_point.column;
}

EXPORT size_t
matrix_max_value(struct matrix * matrix)
{
  struct point max_point = matrix_max_pos(matrix);
  return max_point.value;
}
