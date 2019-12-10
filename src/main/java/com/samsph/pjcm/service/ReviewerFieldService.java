package com.samsph.pjcm.service;
import com.samsph.pjcm.model.EditorField;
import com.samsph.pjcm.model.ReviewerField;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;

import java.util.List;
import java.util.Optional;


public interface ReviewerFieldService {

     void addReviewerField(ReviewerField reviewerField);

     void updateReviewerField(ReviewerField reviewerField);

     void deleteReviewerField(int id);

     Optional<ReviewerField> findReviewerField(int id);

    List<ReviewerField> findByReviewerUid(Integer reviewerUid);

     Page<ReviewerField> findReviewerFieldsByReviewerUid(int reviewerUid, PageRequest pageRequest);

     Page<ReviewerField> findReviewerFieldsByFieldId(int fieldId, PageRequest pageRequest);
}
