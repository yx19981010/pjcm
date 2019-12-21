package com.samsph.pjcm.service.impl;

import cn.hutool.core.bean.BeanUtil;
import cn.hutool.core.bean.copier.CopyOptions;
import com.samsph.pjcm.config.constant.ErrMsg;
import com.samsph.pjcm.config.exception.CustomException;
import com.samsph.pjcm.config.exception.CustomExceptionType;
import com.samsph.pjcm.dao.JournalRepository;
import com.samsph.pjcm.model.Journal;
import com.samsph.pjcm.query.JournalQuery;
import com.samsph.pjcm.service.JournalService;
import org.dozer.Mapper;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;
import java.util.Date;
import java.util.Optional;

/**
 * @author hujiahao
 */

@Service
public class JournalServiceImpl implements JournalService {

    @Resource
    private JournalRepository journalRepository;

    @Resource
    private Mapper dozerMapper;

    @Override
    public Journal saveJournal(JournalQuery journalQuery, int creatorId) {
        // 查询是否已存在该年、月、卷、期的期刊
        int year = journalQuery.getYear();
        int month = journalQuery.getMonth();
        int volume = journalQuery.getVolume();
        int no = journalQuery.getNumber();
        Optional<Journal> journalOptional = journalRepository.findByYearAndMonthAndVolumeAndNumber(year, month, volume, no);

        // 若存在，则抛出异常
        if (journalOptional.isPresent()) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.JOURNAL_EXISTS);
        }

        // 若不存在，则保存该期刊信息
        Journal journal = dozerMapper.map(journalQuery, Journal.class);

        // 设置创建时间、投稿计数、创建者id
        journal.setCreateTime(new Date());
        journal.setCreateByUid(creatorId);
        journal = journalRepository.save(journal);

        return journal;
    }

    @Override
    public Journal getJournal(int id) {
        return fetchJournal(id);
    }

    @Override
    public Journal getJournal(int year, int month, int vol, int no) {
        return fetchJournal(year, month, vol, no);
    }

    @Override
    public Page<Journal> getAll(int page, int size, boolean ascend) {
        Sort.Direction direction = ascend ? Sort.Direction.ASC : Sort.Direction.DESC;

        // 请求分页从1开始计数，JPA分页从0计数，故此处进行减1
        PageRequest pageRequest = PageRequest.of(page - 1, size, direction,"id");

        // 查询分页结果
        return journalRepository.findAll(pageRequest);
    }

    @Override
    public void deleteJournal(int id) {
        fetchJournal(id);
        journalRepository.deleteById(id);
    }

    @Override
    public void updateJournal(Journal journal) {
        journalRepository.save(journal);
    }

    private Journal fetchJournal(int id) {
        // 查询是否已存在该id的期刊
        Optional<Journal> journalOptional = journalRepository.findById(id);

        // 若不存在则抛出异常
        if (journalOptional.isEmpty()) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.JOURNAL_NOT_FOUND);
        }

        return journalOptional.get();
    }

    private Journal fetchJournal(int year, int month, int vol, int no) {
        // 查询是否已存在该年月卷期的期刊
        Optional<Journal> journalOptional = journalRepository.findByYearAndMonthAndVolumeAndNumber(year, month, vol, no);

        // 若不存在则抛出异常
        if (journalOptional.isEmpty()) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.JOURNAL_NOT_FOUND);
        }

        return journalOptional.get();
    }
}
