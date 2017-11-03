# -*- coding:utf-8 -*-

from bs4 import BeautifulSoup
import urllib.request
import requests
import re




# url 가져오는 함수
def getting_url():
    out = open("조선일보_사설URL.txt", 'w')

    # 3월 4일부터 5월 7일까지 2달간 아티클 주소 뽑기.
    for i in range(44):
        url = 'http://news.chosun.com/svc/list_in/list.html?catid=617&pn=' + str(i + 1)
        source_code = requests.get(url)
        text_f = source_code.text
        soup = BeautifulSoup(text_f, 'lxml')
        cnt = 0;
        for title_list in soup.find_all("dd", {"class": "desc"}):

            title_list2 = title_list.find_next("a")
            href = title_list2.get("href")
            print(href, file=out)
    out.close()
#




# 받은 url로 크롤링하는 함수

def get_text(URL):
    source_code_from_URL = urllib.request.urlopen(URL)
    soup = BeautifulSoup(source_code_from_URL, 'lxml', from_encoding='utf-8')
    text_title = soup.find_all("h1",id = 'news_title_text_id')
    text_title_clean = re.search('\] (.*?)\</', str(text_title))
    text_title = str(text_title_clean.group(1))
    print(text_title)
    text = soup.find_all("div", {"class","par"})
    text = text_title + "\n" + str(text)+"\n\n"
    return str(text)

    # text = ''
    # for item in soup.find_all('div', id='article_body'):
    #     text = text + str(item.find_all(text=True))
    #
    # return text
  #




  # 클리닝 함수
def clean_text(text):
    cleaner1 = re.compile('<.*?>')
    remove_tag = re.sub(cleaner1, '', text)
    cleaner2 = re.compile('[.*?]')
    remove_tag2 = re.sub(cleaner2, '', remove_tag)
    remove_num = re.sub('\d', '', remove_tag2)
    remove_sasul = re.sub('사설', '', remove_num)
    cleaned_text1 = re.sub('[a-zA-Z]', '', remove_sasul)
    cleaned_text2 = re.sub('[\{\}\[\]\/?.,;:|\)*~`!^\-_+<>@\#$%&\\\=\(\'\"]', '', cleaned_text1)
    return cleaned_text2
#





# 메인 함수
def main():
    # url 가져오기
    getting_url()

    open_url = open("조선일보_사설URL.txt", "r")
    open_output_file = open("조선일보 사설_text.txt", 'w', encoding='utf8')

    # url에서 \n 없애기
    url = []
    lines = open_url.readlines()
    for line in lines:
        line = line[:-1]
        url.append(line)

    for url_i in url:
        print(url.index(url_i)+1, "번째 기사")

        # 제목 쓰기
        open_output_file.write(str(url.index(url_i)+1) + "번째 기사\n")

        # 받은 url로 크롤링하는 함수
        result_text = get_text(url_i)

        # 클리닝 함수
        result_text = clean_text(result_text)

        # 클리닝 된 것 파일에 쓰기
        open_output_file.write(result_text)

    open_output_file.close()


if __name__ == '__main__':
    main()
