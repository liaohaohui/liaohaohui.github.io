def func ( index_no ):
    x0 = int ( index_no [5:0: -1])
    x1 = (1103515245 * x0 + 12345) % 2147483648
    return int (4 * x1 / 2147483648 + 1)

if __name__ == '__main__':
    exam_id = 'U12345EBAMF' # Change to your Index Number
    task = func ( exam_id )
    print ( task )
