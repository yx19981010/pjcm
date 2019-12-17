package com.samsph.pjcm.dao;

import com.samsph.pjcm.model.EditorField;
import com.samsph.pjcm.model.ReviewerField;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;

import java.util.List;

public interface ReviewerFieldRepository extends JpaRepository<ReviewerField,Integer>, JpaSpecificationExecutor<ReviewerField> {
    /**
     * 根据审稿人的id得到审稿人领域列表
     * @param reviewerUid 审稿人id
     * @return List<ReviewerField>
     */
    List<ReviewerField> findByReviewerUid(Integer reviewerUid);

    /**
     * 根据领域id得到审稿人领域列表
     * @param fieldId 领域id
     * @return List<ReviewerField>
     */
    List<ReviewerField> findByField(Integer fieldId);
}
