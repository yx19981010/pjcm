package com.samsph.pjcm.dao;

import com.samsph.pjcm.model.EditorField;
import com.samsph.pjcm.model.ReviewerField;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;

import java.util.List;

public interface ReviewerFieldRepository extends JpaRepository<ReviewerField,Integer>, JpaSpecificationExecutor<ReviewerField> {
    List<ReviewerField> findByReviewerUid(Integer reviewerUid);

    List<ReviewerField> findByField(Integer fieldId);
}
