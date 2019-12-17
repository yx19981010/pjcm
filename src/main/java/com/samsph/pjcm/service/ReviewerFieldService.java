package com.samsph.pjcm.service;
import com.samsph.pjcm.model.ReviewerField;
import com.samsph.pjcm.vo.ReviewerFieldVoGet;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;

import java.util.List;
import java.util.Optional;


public interface ReviewerFieldService {

     void addReviewerField(ReviewerField reviewerField);

     void updateReviewerField(ReviewerField reviewerField);

     void deleteReviewerField(int id);

     Optional<ReviewerField> findReviewerField(int id);

     List<ReviewerField> findByReviewerUid(Integer reviewerUid);

     List<ReviewerField> findByFieldId(Integer fieldId);

     List<ReviewerFieldVoGet> findAll();

     Page<ReviewerField> findReviewerFieldsByFieldId(int fieldId, Pageable pageable);
}
