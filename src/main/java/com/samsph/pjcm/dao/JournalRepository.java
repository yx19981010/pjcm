package com.samsph.pjcm.dao;

import com.samsph.pjcm.model.Journal;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import java.util.Optional;

/**
 * 期刊表数据操作接口
 *
 * @author hujiahao
 */
public interface JournalRepository extends JpaRepository<Journal, Integer> {
    /**
     * 通过年月卷期查询期刊记录
     * @param year 年号
     * @param month 月号
     * @param volume 卷号
     * @param number 期号
     * @return Optional<Journal>
     */
    Optional<Journal> findByYearAndMonthAndVolumeAndNumber(int year, int month, int volume, int number);

    /**
     * 以分页的方式查询所有期刊
     * @param page 分页请求
     * @return Page<Journal>
     */
    @Override
    @Query(value = "SELECT * FROM journal",
            countQuery = "SELECT count(*) FROM journal",
            nativeQuery = true)
    Page<Journal> findAll(Pageable page);
}
