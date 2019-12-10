package com.samsph.pjcm.service.impl;
import com.samsph.pjcm.dao.ReviewerFieldRepository;
import com.samsph.pjcm.model.EditorField;
import com.samsph.pjcm.model.ReviewerField;
import com.samsph.pjcm.service.ReviewerFieldService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Example;
import org.springframework.data.domain.ExampleMatcher;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.Optional;

@Service
@Transactional
public class ReviewerFieldServiceImpl implements ReviewerFieldService {
    @Autowired
    private ReviewerFieldRepository reviewerFieldRepository;

    @Override
    public void addReviewerField(ReviewerField reviewerField) {

        reviewerFieldRepository.save(reviewerField);
    }

    @Override
    public void updateReviewerField(ReviewerField reviewerField) {

        reviewerFieldRepository.save(reviewerField);
    }

    @Override
    public void deleteReviewerField(int id) {

        reviewerFieldRepository.deleteById(id);
    }

    @Override
    public Optional<ReviewerField> findReviewerField(int id) {

        return reviewerFieldRepository.findById(id);
    }

    @Override
    public List<ReviewerField> findByReviewerUid(Integer reviewerUid) {
        return reviewerFieldRepository.findByReviewerUid(reviewerUid);
    }

    @Override
    public Page<ReviewerField> findReviewerFieldsByReviewerUid(int reviewerUid, PageRequest pageRequest) {
        ReviewerField reviewerField = new ReviewerField();
        reviewerField.setReviewerUid(reviewerUid);
        ExampleMatcher exampleMatcher = ExampleMatcher.matching().withMatcher("reviewerUid",ExampleMatcher.GenericPropertyMatchers.exact());
        Example<ReviewerField> example = Example.of(reviewerField,exampleMatcher);
        return reviewerFieldRepository.findAll(example,pageRequest);
    }

    @Override
    public Page<ReviewerField> findReviewerFieldsByFieldId(int fieldId, PageRequest pageRequest) {
        ReviewerField reviewerField = new ReviewerField();
        reviewerField.setField(fieldId);
        ExampleMatcher exampleMatcher = ExampleMatcher.matching().withMatcher("field",ExampleMatcher.GenericPropertyMatchers.exact());
        Example<ReviewerField> example = Example.of(reviewerField,exampleMatcher);
        return reviewerFieldRepository.findAll(example,pageRequest);
    }
}
