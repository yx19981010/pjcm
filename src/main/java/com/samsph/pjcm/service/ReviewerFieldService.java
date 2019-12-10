package com.samsph.pjcm.service;
import com.samsph.pjcm.model.EditorField;
import com.samsph.pjcm.model.ReviewerField;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;

import java.util.Optional;


public interface ReviewerFieldService {

    public void addReviewerField(ReviewerField reviewerField);

    public void updateReviewerField(ReviewerField reviewerField);

    public void deleteReviewerField(int id);

    public Optional<ReviewerField> findReviewerField(int id);

    public Page<ReviewerField> findReviewerFieldsByReviewerUid(int reviewerUid, PageRequest pageRequest);

    public Page<ReviewerField> findReviewerFieldsByFieldId(int fieldId, PageRequest pageRequest);
}
