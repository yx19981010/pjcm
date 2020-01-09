package com.samsph.pjcm.dao;

import com.samsph.pjcm.model.PostNoOffset;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;

/**
 * @author hujiahao
 */
public interface PostNoOffsetRepository extends JpaRepository<PostNoOffset, Integer> {

    /**
     *  通过年号查稿件编号的四位序号起始值
     * @param year 年号
     * @return Optional<PostNoOffset>
     */
    Optional<PostNoOffset> findByYear(int year);
}
