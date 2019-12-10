package com.samsph.pjcm.dao;

import com.samsph.pjcm.model.EditorField;
import com.samsph.pjcm.model.ReviewerField;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

public interface ReviewerFieldRepository extends JpaRepository<ReviewerField,Integer> {
    List<ReviewerField> findByReviewerUid(Integer reviewerUid);
}
