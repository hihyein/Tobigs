# 사전 불러오기
import pandas as pd
file = open('C:\\Users\\Hyein\\Desktop\\Tobigs\\2017-1\\project\\감성사전\\점수_1.txt', 'r')
dictionary = file.read()
dictionary = dictionary.split('\n')
dictionary


dictionary = dictionary[1:len(dictionary)-1]



# 뉴스기사 불러오기
from konlpy.tag import Kkma
from konlpy.utils import pprint
kkma = Kkma()

file = open('중앙일보 사설_text_200개샘플_2.txt', 'r', encoding='utf-8')
text = file.read()
text = text.split('\n\n')
text



polarity = dictionary



score = []

for k in range(0, len(text)):

    # 0번째 뉴스기사 형태소분석
    txt_k = kkma.pos(text[k])
    #txt_k

    # 형태소분석(튜플) -> polarity에 있는 형태로 이어붙이기
    txt_k_1 = []
    for i in range(0,len(txt_k)):
        txt_k_1.append(txt_k[i][0] + '/' + txt_k[i][1])
    # txt_k_1

    # kkma -> 세종
    txt_new = ""
    for i in range(0, len(txt_k_1)):
        txt_new = txt_new + txt_k_1[i] + ';'
    txt_new = txt_new.replace("NNM", "NNB")
    txt_new = txt_new.replace("VXV", "VX")
    txt_new = txt_new.replace("VXA", "VX")
    txt_new = txt_new.replace("MDT", "MM")
    txt_new = txt_new.replace("MDN", "MM")
    txt_new = txt_new.replace("EPH", "EP")
    txt_new = txt_new.replace("EPT", "EP")
    txt_new = txt_new.replace("EPP", "EP")
    txt_new = txt_new.replace("EFN", "EF")
    txt_new = txt_new.replace("EFQ", "EF")
    txt_new = txt_new.replace("EFO", "EF")
    txt_new = txt_new.replace("EFA", "EF")
    txt_new = txt_new.replace("EFI", "EF")
    txt_new = txt_new.replace("EFR", "EF")
    txt_new = txt_new.replace("ECE", "EC")
    txt_new = txt_new.replace("ECD", "EC")
    txt_new = txt_new.replace("ECS", "EC")

    # 완성된 원본 파일!!!
    txt_0 = txt_new.split(';')
    txt_0 = txt_0[0:len(txt_0)-1]
    #txt_0

    # 3개씩
    txt_3 = []
    for i in range(0, len(txt_0)-2):
        txt_3.append(txt_0[i] + ';' + txt_0[i+1] + ';' + txt_0[i+2])
    # txt_3

    dic = []
    txt_index = []
    pol_index = []
    for i in range(0, len(txt_3)):
        for j in range(0, len(polarity)):
            if polarity[j].find(txt_3[i]) == 0:
                txt_index.append(i)
                pol_index.append(j)
                dic.append(polarity[j])
                # print(i,j)
                break

    # 찾은 단어 제거하기
    txt_m = txt_0
    r_txt_index = txt_index
    r_txt_index.reverse()
    for i in r_txt_index:
        del txt_m[i:i+3]
    # txt_m



    # 2개씩
    txt_2 = []
    for i in range(0, len(txt_m)-1):
        txt_2.append(txt_m[i] + ';' + txt_m[i+1])
    # txt_2

    txt_index = []
    pol_index = []
    for i in range(0, len(txt_2)):
        for j in range(0, len(polarity)):
            if polarity[j].find(txt_2[i]) == 0:
                txt_index.append(i)
                pol_index.append(j)
                dic.append(polarity[j])
                # print(i,j)
                break

    # 찾은 단어 제거하기
    r_txt_index = txt_index
    r_txt_index.reverse()
    for i in r_txt_index:
        del txt_m[i:i+2]
    # txt_m



    # 1개씩
    txt_1 = txt_m

    txt_index = []
    pol_index = []
    for i in range(0, len(txt_1)):
        for j in range(0, len(polarity)):
            if polarity[j].find(txt_1[i]) == 0:
                txt_index.append(i)
                pol_index.append(j)
                dic.append(polarity[j])
                # print(i,j)
                break

    # 찾은 단어 제거하기
    r_txt_index = txt_index
    r_txt_index.reverse()
    for i in r_txt_index:
        del txt_m[i]
    # txt_m



    # 점수 합계 내기
    sum = 0
    for i in range(0, len(dic)):
        sum += float(dic[i].split(',')[-1])
    print(k)
    score.append(sum)



score



f = open("score_pol_int.txt", 'w')
for i in range(0, len(score)):
    f.write(str(score[i]) + '\n')
f.close()
