library(readr)
library(tidyr)
library(dplyr)
library(stringr)
library(purrr)
library(igraph)

interested = c("List of Departments that are interesting or relevant to you, use department code. eg. "PSYC", "CCAS")

taken = c("Classes you've already taken, use department and course code",
          "eg. "PSYC 1001", "PHIL 2045", "BISC 1007", "SMPA 2112", "SMPA 2113"")

# Short list of required classes, not complete bc I got lazy, will finish up later (probably not). 
requirements = list(
    arts = c("CMUS 3123"),
    global = c("PSC 1001"),
    civic = c("BISC 1007"),
    univ.writing = c("UW 1020"),
    quant.reason = c("ECON 1001", "CCAS 1200", "GEOG 3133", "MATH 007", "MATH 1009", "MATH 1010", "MATH 1021", "MATH 1031"),
    sci.reason = c("WGSS 6265", "STAT 053", "SOC 6265", "PUBH 8534", "PUBH 8525", "PSCS 3110", "PHYS 1026", "PHYS 1025", "PHYS 1022", "PHYS 1021", "PHYS 1012", "PHYS 1011", "PHYS 1007", "PHYS 1003W", "PHYS 1003", "PHYS 022W", "ORSC 8265", "HONR 1034", "HONR 1033", "GTCH 3202", "GEOL 1005", "GEOL 1002", "GEOL 1001", "GEOG 3133", "GEOG 1002", "GEOG 002", "FORS 2107", "ENVR 1098", "CHEM 2154", "CHEM 1112", "CHEM 1111", "CHEM 1004", "CHEM 1003", "CE 6502", "CE 3520", "BISC 1334", "BISC 1125", "BISC 1112W", "BISC 1112", "BISC 1111", "BISC 1008", "BISC 1007", "BISC 1006", "BISC 1005", "BIOC 6241", "ASTR 1002", "ASTR 1001", "ANTH 3412W", "ANTH 3412", "ANTH 1001", "ANTH 001", "ANAT 2151", "ANAT 2131" ),
    crit.think.soc = c("COMM 1040", "ECON 1011", "PSC 002"),
    crit.think.hum = c("PHIL 1051", "PHIL 2131", "AMST 2410", "AMST 2610", "AMST 2620"),
    oral.comm = c("COMM 1040", "DNSC 4233W", "SLHS 1011")
)

total.reqs = list(
    arts = 1,
    global = 1,
    civic = 1,
    univ.writing = 1,
    quant.reason = 1,
    sci.reason = 2,
    crit.think.soc = 2,
    crit.think.hum = 2,
    oral.comm = 1
)

cl <-read_csv("gw_courses_fall2025_full.csv", show_col_types = FALSE) 

cl <- cl %>%
    separate(
        `Subject+Course`,
        into = c("Subject", "Course"),
        sep = " ", extra = "merge"
        ) %>%
    mutate(
        sub_course = paste(Subject, Course, sep = " "),
        Credits = as.numeric(str_extract(Credits, "\\d+")),
        )

# Merges all duplicate classes (original course list lists each course + time slot.) I plan to introduce a version of this that can give you your exact schedule based on your interests
# for now just raw classes
cl.unique <- cl %>%
    arrange(Subject, Course, Title, desc(Status == "OPEN")) %>%
    distinct(Subject, Course, Title, .keep_all = TRUE)

cl.unique <- cl.unique %>%
    group_by(Subject, Course) %>%
    arrange(desc(nchar(Title)), .by_group = TRUE) %>%
    mutate(
        lab_discussion = n() > 1,
        is_main_course = row_number() == 1
    ) %>%
    filter(is_main_course == TRUE) %>%
    ungroup() %>%
    select(-is_main_course)

# Create completed dataframe from taken courses
completed <- cl.unique %>%
    filter(sub_course %in% taken) %>%
    mutate(
        completed = TRUE
    )

extract_prereq <- function(text, sub_course) {
    if(is.na(text)) return(NA)

    pattern <- paste(sub_course, collapse = "|")
    matches <- str_extract_all(text, pattern)

    if(length(unlist(matches)) > 0) {
        return(paste(unique(unlist(matches)), collapse = ", "))
    } else {
        return(NA)
    }
}


cl.prereq <- cl.unique %>%
    mutate(
        prerequisites = pmap_chr(
            list(
                ifelse(is.na(Comments), "", Comments),
                ifelse(is.na(Description), "", Description),
                sub_course 
            ),
            function(c, d, current_course) { 
                prereqs <- extract_prereq(paste(c, d), cl$sub_course)
                if(is.na(prereqs)) return(NA)
                
                prereqs <- str_split(prereqs, ", ")[[1]]
                prereqs <- prereqs[prereqs != current_course]
                if(length(prereqs) > 0) return(paste(prereqs, collapse=", "))
                return(NA)
            }
        )
    )


# First, create a graph of prerequisite relationships
prereq_relationships <- cl.prereq %>%
    filter(!is.na(prerequisites)) %>%
    select(course = sub_course, prerequisites) %>%
    mutate(prerequisites = str_split(prerequisites, ", ")) %>%
    unnest(prerequisites)

# Create a directed graph
prereq_graph <- graph_from_data_frame(
    d = prereq_relationships %>% select(prerequisites, course),
    directed = TRUE
)

# Function to get all prerequisites (direct and indirect) for a course
get_all_prerequisites <- function(course_code, graph) {
    # If course not in graph, return NA
    if (!course_code %in% V(graph)$name) return(NA)
    
    # Get vertex ID for the course
    v_id <- which(V(graph)$name == course_code)
    
    # Get all ancestors (prerequisites and their prerequisites)
    ancestors <- subcomponent(graph, v_id, mode = "in")
    ancestor_names <- V(graph)$name[ancestors]
    
    # Remove the course itself from the list
    ancestor_names <- ancestor_names[ancestor_names != course_code]
    
    # Return comma-separated list, or NA if empty
    if (length(ancestor_names) > 0) {
        return(paste(ancestor_names, collapse = ", "))
    } else {
        return(NA)
    }
}

# Function to get all dependencies (courses that require this course)
get_all_dependencies <- function(course_code, graph) {
    # If course not in graph, return NA
    if (!course_code %in% V(graph)$name) return(NA)
    
    # Get vertex ID for the course
    v_id <- which(V(graph)$name == course_code)
    
    # Get all descendants (courses that depend on this one)
    descendants <- subcomponent(graph, v_id, mode = "out")
    descendant_names <- V(graph)$name[descendants]
    
    # Remove the course itself from the list
    descendant_names <- descendant_names[descendant_names != course_code]
    
    # Return comma-separated list, or NA if empty
    if (length(descendant_names) > 0) {
        return(paste(descendant_names, collapse = ", "))
    } else {
        return(NA)
    }
}

# Add both columns to cl.prereq
cl.prereq <- cl.prereq %>%
    mutate(
        all_prerequisites = sapply(sub_course, function(x) get_all_prerequisites(x, prereq_graph)),
        prerequisites_count = sapply(all_prerequisites, function(x) {
            if(is.na(x)) {
                return(0)  # No prerequisites
            } else {
                return(length(strsplit(x, ", ")[[1]]))  # Count items in comma-separated list
            }
        }),
        dependencies = sapply(sub_course, function(x) get_all_dependencies(x, prereq_graph)),
        dependants_count = sapply(dependencies, function(x) {
            if(is.na(x)) {
                return(0)  # No dependencies
            } else {
                return(length(strsplit(x, ", ")[[1]]))  # Count items in comma-separated list
            }
        })
    )


cl.filtered <- cl.prereq %>%
    filter(Subject %in% interested) %>%
    filter(!(Title %in% c("Research", "Independent Study", "Thesis Research", "Graduate Seminar",
                        "Independent Study", "Capstone", "Capstone Project", "Capstone Research"))
    ) %>%
    filter(!(Credits == 0)) %>%
    filter(!grepl("Registration restricted to graduate students only", Comments))

cl.final <- cl.filtered %>%
    mutate(
        completed = sub_course %in% taken,  # Use taken directly instead of completed$sub_course
        credits = ifelse(is.na(Credits), 0, Credits),
        subject = Subject,
        course = Course,
        title = Title,
        description = Description,
        comments = Comments,
        # Add new column for prereqs completed
        prereqs_completed = sapply(prerequisites, function(x) {
            # If no prerequisites, return TRUE
            if(is.na(x)) {
                return(TRUE)
            } else {
                # Split prerequisites into a vector
                prereq_list <- str_split(x, ", ")[[1]]
                # Check if all prerequisites are in the taken vector
                all(prereq_list %in% taken)  # Use taken instead of completed$sub_course
            }
        })
    ) %>%
    select(completed, lab_discussion, subject, course, title, credits, Campus, description, comments, 
           prerequisites, all_prerequisites, prerequisites_count, dependencies, dependants_count, 
           prereqs_completed)

cl.prefilt <- cl.final %>%
    filter(prereqs_completed == TRUE)

# cl.prereq <- cl.filtered %>%
#    filter(grepl("Prerequisites|Prerequisite|Prereq|prereq|pre-req|Pre-req", Comments) |
#           grepl("Prerequisites|Prerequisite|Prereq|prereq|pre-req|Pre-req", Description)) 

# cl.noprereq <- cl.filtered %>%
#     filter(!(grepl("Prerequisites|Prerequisite|Prereq|prereq|pre-req|Pre-req", Comments) |
#              grepl("Prerequisites|Prerequisite|Prereq|prereq|pre-req|Pre-req", Description)))

subject.count <- cl.unique %>%
    group_by(Subject) %>%
    summarize(Count = n()) %>%
    arrange(desc(Count))

write.csv(cl.final, "gw_courses_fall2025_final.csv", row.names = FALSE)
write.csv(cl.prefilt, "gw_courses_fall2025_prereq_adjusted.csv", row.names = FALSE)



