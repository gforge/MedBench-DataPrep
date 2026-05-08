"""GraphQL query strings for review campaign exports."""

REVIEW_CAMPAIGNS_QUERY = """
query ReviewCampaigns($onlyActive: Boolean!) {
  reviewCampaigns(onlyActive: $onlyActive) {
    id
    label
    description
    reviewType
    activationDate
    targetReviews
    deactivationDate
    isActive
    createdAt
    charts {
      id
      name
      specialty
      language
      notes {
        id
        type
        date
        time
        author
        content
        chartTranslationId
      }
      medications {
        id
        chartTranslationId
        atcCode
        medication
        wayOfAdministration
        strength
        unit
        timesPerDay
        date
      }
      labValues {
        id
        chartTranslationId
        labTest
        value
        unit
        referenceInterval
        date
        time
      }
      summaries {
        id
        chartTranslationId
        generatedBy
        text
        createdBy
        createdAt
        hasReviews
        reviewCount
      }
    }
  }
}
"""

ALL_REVIEWS_QUERY = """
query AllReviews {
  allReviews {
    id
    chartTranslationId
    summaryId
    userId
    rating
    seconds2review
    reviewType
    createdAt
    experienceSnapshot
    campaign {
      id
      label
      description
      reviewType
      activationDate
      targetReviews
      deactivationDate
      createdAt
    }
    user {
      id
      userMainEmail
      firstName
      middleName
      lastName
      specialtyExperiences {
        specialty
        level
        yearStarted
        yearsAtLevel
        isActive
      }
      languageExperiences {
        language
        subLanguage
        proficiency
        workedInHealthcare
        yearsInHealthcare
      }
    }
  }
}
"""

CHARTS4EXPORT_QUERY = """
query ExportCharts($allVersions: Boolean) {
  charts4export(allVersions: $allVersions) {
    id
    name
    specialty
    factCount
    factStatus
    facts {
      id
      question
      answer
      createdAt
      createdBy
    }
    charts {
      id
      caseId
      name
      specialty
      language
      subLanguage
      isOriginal
      version
      status
      createdAt
      createdBy
      notes {
        id
        type
        date
        time
        author
        content
        chartTranslationId
      }
      medications {
        id
        chartTranslationId
        atcCode
        medication
        wayOfAdministration
        strength
        unit
        timesPerDay
        date
      }
      labValues {
        id
        chartTranslationId
        labTest
        value
        unit
        referenceInterval
        date
        time
      }
      summaries {
        id
        chartTranslationId
        generatedBy
        generatorType
        text
        status
        createdBy
        createdAt
      }
    }
  }
}
"""
