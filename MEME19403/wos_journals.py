fp = open("WOS Journals Quartile Ranking for 2020.txt", "rt")
cnt = 0
data_entry = []
for i in fp.readlines():
    i = i.strip()
    if len(i) > 0:
        cnt += 1
        #print(f"{cnt}: {i}")
        if cnt >= 6:
            if len(i) == 9 and i[4]=='-':   # ISBN
                data_entry.append([i])
            elif i[0] == 'Q' and (i[1] in '1234'):
                data_entry[len(data_entry)-1].append(i)
            else:
                data_entry[len(data_entry)-1].append(i)

import pandas as pd
#df = pd.DataFrame(data_entry)
#print(df)
#df.to_csv("WOS_Journals2020.csv")
for i in data_entry:
    print(i)

