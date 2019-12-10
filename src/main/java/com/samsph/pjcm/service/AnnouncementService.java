package com.samsph.pjcm.service;

import com.samsph.pjcm.model.Announcement;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;

import java.util.Optional;

public interface AnnouncementService {
    /**
     * 添加公告
     * @param announcement
     */
    public void addAnnouncement(Announcement announcement);

    /**
     * 更新公告
     * @param announcement
     */
    public void updateAnnouncement(Announcement announcement);

    /**
     * 删除公告
     * @param id
     */
    public void deleteAnnouncement(int id);

    /**
     * 得到公告列表
     * @param pageRequest
     * @return
     */
    public Page<Announcement> findAnnouncements(PageRequest pageRequest);

    /**
     * 得到单个公告
     * @param id
     * @return
     */
    public Optional<Announcement> findAnnouncement(int id);
}
