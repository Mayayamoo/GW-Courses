import requests
from bs4 import BeautifulSoup
import csv
import time
from urllib.parse import urljoin
import re

# Constants
BASE_URL = "https://my.gwu.edu/mod/pws/"
BULLETIN_URL_BASE = "https://bulletin.gwu.edu/search/?P="
DEPARTMENT_LIST_URL = urljoin(BASE_URL, "subjects.cfm?campId=1&termId=202503")
MOUNT_VERNON_URL =  urljoin(BASE_URL, "subjects.cfm?campId=4&termId=202503")
VT_URL = urljoin(BASE_URL, "subjects.cfm?campId=2&termId=202503")
OFF_CAMPUS_URL = urljoin(BASE_URL, "subjects.cfm?campId=3&termId=202503")
ONLINE_URL = urljoin(BASE_URL, "subjects.cfm?campId=7&termId=202503")

# Cache bulletin URLs so we don't hit the same one twice
bulletin_cache = {}

# Step 1: Get all department links from a specific campus
def get_department_links(campus_url, campus_name):
    response = requests.get(campus_url)
    soup = BeautifulSoup(response.content, "html.parser")
    
    # Use the existing selector but deduplicate the links
    links = soup.select(".subjectsMain a[href]")
    unique_links = set([urljoin(BASE_URL, link['href']) for link in links])
    dept_links = list(unique_links)
    
    print(f"🔗 Found {len(dept_links)} department pages for {campus_name}")
    return [(link, campus_name) for link in dept_links]

# Step 2: Get bulletin course description from subject+course like "ACA 6205"
def get_bulletin_description(subject_course):
    if subject_course in bulletin_cache:
        return bulletin_cache[subject_course]

    url = BULLETIN_URL_BASE + subject_course.replace(" ", "+")
    try:
        r = requests.get(url)
        soup = BeautifulSoup(r.content, "html.parser")
        desc = soup.select_one(".courseblockdesc")
        text = desc.get_text(" ", strip=True) if desc else ""
        bulletin_cache[subject_course] = text
        time.sleep(0.5)  # avoid overloading the bulletin server
        return text
    except Exception as e:
        print(f"⚠️ Failed to get bulletin description for {subject_course}: {e}")
        return ""

# Step 3: Parse a single department page
def scrape_department(dept_url, campus_name):
    courses = []
    try:
        # Extract subjId from URL
        from urllib.parse import urlparse, parse_qs
        parsed_url = urlparse(dept_url)
        params = parse_qs(parsed_url.query)
        subj_id = params.get("subjId", [""])[0]

        # Step 1: Detect total pages
        resp = requests.get(dept_url)
        soup = BeautifulSoup(resp.content, "html.parser")

        # Extract pagination count from javascript:goToPage('2') links
        page_links = soup.find_all("a", href=True)
        pages = set()
        for link in page_links:
            match = re.search(r"goToPage\('(\d+)'\)", link["href"])
            if match:
                pages.add(int(match.group(1)))
        total_pages = max(pages) if pages else 1

        print(f"   📄 Pages: {total_pages}")

        # Step 2: Loop through each page using POST requests
        for page in range(1, total_pages + 1):
            print(f"      🔄 Fetching page {page}")
            page_resp = requests.post(
                dept_url,
                data={"pageNum": page},
                headers={"User-Agent": "Mozilla/5.0"},
            )
            page_soup = BeautifulSoup(page_resp.content, "html.parser")
            tables = page_soup.find_all("table", class_="courseListing")

            for table in tables:
                rows1 = table.find_all("tr", class_="crseRow1")
                rows2 = table.find_all("tr", class_="crseRow2")

                for r1, r2 in zip(rows1, rows2):
                    cols = r1.find_all("td")
                    if len(cols) < 10:
                        continue

                    subject_course = cols[2].get_text(" ", strip=True)
                    course_link = cols[2].find("a")
                    description = ""
                    if course_link and subject_course:
                        description = get_bulletin_description(subject_course)

                    comment = r2.get_text(" ", strip=True)

                    course = {
                        "Status": cols[0].get_text(strip=True),
                        "CRN": cols[1].get_text(strip=True),
                        "Subject+Course": subject_course,
                        "Section": cols[3].get_text(strip=True),
                        "Title": cols[4].get_text(strip=True),
                        "Credits": cols[5].get_text(strip=True),
                        "Instructor": cols[6].get_text(strip=True),
                        "Room": cols[7].get_text(" ", strip=True),
                        "Days/Times": cols[8].get_text(" ", strip=True),
                        "Dates": cols[9].get_text(strip=True),
                        "Comments": comment,
                        "Description": description,
                        "Campus": campus_name  # Add campus information
                    }

                    courses.append(course)

            time.sleep(0.4)  # avoid hammering server

    except Exception as e:
        print(f"❌ Error scraping {dept_url}: {e}")
    return courses


# Step 4: Scrape all and write CSV
def scrape_all_courses(filename="gw_courses_fall2025_full.csv"):
    # Define campus URLs with names
    campus_sources = {
        "Main Campus": DEPARTMENT_LIST_URL,
        "Mount Vernon": MOUNT_VERNON_URL,
        "Virginia Tech": VT_URL,
        "Off Campus": OFF_CAMPUS_URL,
        "Online": ONLINE_URL
    }
    
    all_courses = []
    total_dept_links = []
    
    # Get department links from all campuses
    for campus_name, campus_url in campus_sources.items():
        print(f"🏫 Fetching departments for: {campus_name}")
        dept_links = get_department_links(campus_url, campus_name)
        total_dept_links.extend(dept_links)
    
    print(f"🔗 Found {len(total_dept_links)} total department pages across all campuses")
    
    # Scrape all department links
    for idx, (link, campus_name) in enumerate(total_dept_links):
        print(f"📘 ({idx+1}/{len(total_dept_links)}) Scraping department from {campus_name}: {link}")
        dept_courses = scrape_department(link, campus_name)
        print(f"   ✅ {len(dept_courses)} courses found.")
        all_courses.extend(dept_courses)
        time.sleep(1)  # polite delay

    # Save to CSV
    if all_courses:
        keys = list(all_courses[0].keys())
        with open(filename, "w", newline="", encoding="utf-8") as f:
            writer = csv.DictWriter(f, fieldnames=keys)
            writer.writeheader()
            writer.writerows(all_courses)

        print(f"✅ DONE: {len(all_courses)} total courses saved to {filename}")
    else:
        print("⚠️ No courses scraped.")

# Run it
scrape_all_courses()
