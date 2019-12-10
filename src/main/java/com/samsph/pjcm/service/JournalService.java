package com.samsph.pjcm.service;

import com.samsph.pjcm.model.Journal;
import com.samsph.pjcm.query.JournalQuery;
import org.springframework.data.domain.Page;

/**
 * 期刊表service层接口
 *
 * @author hujiahao
 */
public interface JournalService {
    /**
     * 新增期刊
     *
     * @param journalQuery 创建期刊请求
     * @param creatorId    创建者id
     * @return 期刊实体
     */
    Journal saveJournal(JournalQuery journalQuery, int creatorId);


    /**
     * 根据id获取期刊
     *
     * @param id 期刊id
     * @return 期刊实体
     */
    Journal getJournal(int id);


    /**
     * 根据年月卷期号获取期刊
     *
     * @param year  年号
     * @param month 月号
     * @param vol   卷号
     * @param no    期号
     * @return 期刊实体
     */
    Journal getJournal(int year, int month, int vol, int no);


    /**
     * 获取期刊分页
     *
     * @param page   分页页号
     * @param size   分页页面大小
     * @param ascend 是否升序
     * @return 期刊分页
     */
    Page<Journal> getAll(int page, int size, boolean ascend);


    /**
     * 更新期刊
     *
     * @param journal 要更新的期刊对象
     */
    void updateJournal(Journal journal);

    /**
     * 通过id删除期刊
     *
     * @param id 期刊id
     */
    void deleteJournal(int id);
}